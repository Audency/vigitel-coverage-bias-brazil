# -*- coding: utf-8 -*-
"""
Traduz para o ingles as tabelas do manuscrito e as legendas de tabelas e figuras,
dentro do proprio .docx: preserva estrutura, estilos, numeracao e marcadores de
nota. Numeros nao sao redigitados - so a notacao muda (virgula decimal -> ponto,
separador de milhar -> virgula, " a " -> " to " nos intervalos negativos).

USO: python3 tools/traduz_docx.py "<arquivo.docx>" [--dry-run]

Falha e nao grava nada se algum segmento de texto nao tiver traducao no
dicionario (tools/traducoes_tabelas.py).
"""
import sys, os, re, shutil, zipfile
from lxml import etree

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from traducoes_tabelas import TABELAS, LEGENDAS

W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
XML_SPACE = "{http://www.w3.org/XML/1998/namespace}space"
MARCADOR = re.compile(r"^[¹²³⁴⁵⁶⁷⁸⁹⁰\d\*]{1,3}$")


def texto(el):
    return "".join(t.text or "" for t in el.iter(W + "t"))


def e_marcador(r):
    """Run que contem apenas o marcador sobrescrito de nota (¹, ², 1, *)."""
    rPr = r.find(W + "rPr")
    if rPr is None:
        return False
    va = rPr.find(W + "vertAlign")
    if va is None or va.get(W + "val") != "superscript":
        return False
    t = "".join(x.text or "" for x in r.findall(W + "t")).strip()
    return bool(t) and bool(MARCADOR.match(t))


def segmentos(p):
    """[(texto, [runs])] por segmento separado por quebra de linha."""
    segs, cur_t, cur_r = [], "", []
    for r in p.findall(W + "r"):
        if r.find(W + "br") is not None:
            segs.append((cur_t, cur_r)); cur_t, cur_r = "", []
        if e_marcador(r):
            continue
        ts = r.findall(W + "t")
        if ts:
            cur_t += "".join(t.text or "" for t in ts)
            cur_r.append(r)
    segs.append((cur_t, cur_r))
    return [(t, rs) for t, rs in segs if t.strip()]


def numerico(s):
    s = s.strip()
    if not s or not re.search(r"\d", s):
        return False
    return re.sub(r"NA|\bp\b|[\d.,%()\-–—*±≥≤<>=;: a/]", "", s) == ""


def converte_numero(s):
    s = re.sub(r"(?<=\d)\.(?=\d{3}(\D|$))", "\x00", s)   # separador de milhar
    s = re.sub(r"(?<=\d),(?=\d)", ".", s)                 # separador decimal
    s = s.replace("\x00", ",")
    s = re.sub(r"(?<=\d) a (?=-?\d)", " to ", s)          # intervalo com negativo
    return s


def escreve(runs, novo):
    """Poe o texto no primeiro run do segmento e esvazia os demais."""
    primeiro = True
    for r in runs:
        for t in r.findall(W + "t"):
            if primeiro:
                t.text = novo
                if novo != novo.strip():
                    t.set(XML_SPACE, "preserve")
                primeiro = False
            else:
                t.text = ""
        rPr = r.find(W + "rPr")
        if rPr is not None:
            lang = rPr.find(W + "lang")
            if lang is not None and (lang.get(W + "val") or "").startswith("pt"):
                lang.set(W + "val", "en-GB")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    doc = sys.argv[1]
    dry = "--dry-run" in sys.argv

    z = zipfile.ZipFile(doc)
    root = etree.fromstring(z.read("word/document.xml"))
    body = root.find(W + "body")
    blocos = [c for c in body if c.tag in (W + "p", W + "tbl")]

    faltando, n_txt, n_num, n_leg = [], 0, 0, 0

    # 1) tabelas
    for b in blocos:
        if b.tag != W + "tbl":
            continue
        for p in b.iter(W + "p"):
            for t, runs in segmentos(p):
                bruto = t.strip()
                if numerico(bruto):
                    novo = converte_numero(t)
                    if novo != t:
                        escreve(runs, novo); n_num += 1
                elif bruto in TABELAS:
                    escreve(runs, t.replace(bruto, TABELAS[bruto])); n_txt += 1
                else:
                    faltando.append(bruto)

    # 2) legendas de tabelas e figuras
    for b in blocos:
        if b.tag != W + "p":
            continue
        t = texto(b).strip()
        if t in LEGENDAS:
            segs = segmentos(b)
            if len(segs) == 1:
                escreve(segs[0][1], LEGENDAS[t]); n_leg += 1
            else:
                faltando.append("LEGENDA MULTISEGMENTO: " + t)

    print(f"segmentos de texto traduzidos : {n_txt}")
    print(f"celulas numericas reformatadas: {n_num}")
    print(f"legendas traduzidas           : {n_leg}")

    if faltando:
        print("\nSEM TRADUCAO (%d) - nada foi gravado:" % len(faltando))
        for f in sorted(set(faltando)):
            print("  -", f[:160])
        sys.exit(1)

    if dry:
        print("\n--dry-run: documento nao alterado.")
        return

    bak = re.sub(r"\.docx$", " (tabelas PT - backup).docx", doc)
    if not os.path.exists(bak):
        shutil.copy2(doc, bak)
        print("\nbackup:", os.path.basename(bak))

    novo_xml = etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)
    tmp = doc + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as out:
        for item in z.infolist():
            dados = novo_xml if item.filename == "word/document.xml" else z.read(item.filename)
            zi = zipfile.ZipInfo(item.filename, date_time=item.date_time)
            zi.compress_type = item.compress_type
            zi.external_attr = item.external_attr
            zi.internal_attr = item.internal_attr
            zi.create_system = item.create_system
            out.writestr(zi, dados)
    z.close()
    os.replace(tmp, doc)
    print("gravado:", os.path.basename(doc))


if __name__ == "__main__":
    main()
