#!/usr/bin/env python3

"""
Basic build-script to create HTML book from .lhs sources,
mostly using basic templating
"""

import os
import subprocess

def read_file(filepath):
    with open(filepath, "r") as f:
        return f.read()

def open_template(name):
    return read_file(name + ".template")

def apply_template(template, substs):
    t = template
    for (k, v) in substs.items():
        t = t.replace("{" + k + "}", v)
    return t

def write_string_to_file(s, filepath):
    with open(filepath, "w") as f:
        f.write(s)

def render_lhs(lhs_filepath):
    return subprocess.check_output([
        "pandoc", lhs_filepath,
                  "-f", "markdown+lhs",
                  "-t", "html",
                  "--mathjax"
    ]).decode("utf8")

def build_sections(sources):
    for (section_name, chapter_sources) in sources:
        section_out_path = section_name
        if not os.path.exists(section_out_path):
            os.makedirs(section_out_path)
        for (chap_name, chap_source) in chapter_sources:
            chap_content = render_lhs(chap_source)
            chap_templ = open_template("chapter")
            chap = apply_template(chap_templ, { "content": chap_content })
            out_path = "{}/{}.html".format(section_out_path, chap_name)
            write_string_to_file(chap, out_path)


def build_index(sources):
    toc = "<ol>\n"
    for (section_name, chapter_sources) in sources:
        toc += "<li>\n"
        toc += "<div>" + section_name + "</div>\n"
        toc += "<ul>\n"
        for (chapter_name, _) in chapter_sources:
            chapter_path = "{}/{}.html".format(section_name, chapter_name)
            toc += "<li><a href=\"{}\">{}</a></li>\n".format(chapter_path, chapter_name)
        toc += "</ul>\n</li>\n"
    index_template = open_template("index")
    index = apply_template(index_template, { "toc": toc })
    write_string_to_file(index, "index.html")

sources = [
    ("Units", [
        ("Introduction", "../Physics/src/Units/UnitsIntro.lhs"),
        ("Quantities", "../Physics/src/Units/Quantity.lhs"),
        ("Value-level units", "../Physics/src/Units/ValueLevel.lhs"),
        ("Type-level units", "../Physics/src/Units/TypeLevel.lhs"),
    ]),
    ("Vectors", [
        ("Main", "../Physics/src/Vector/Vector.hs")
    ]),
    ("Calculus", [
        ("Main", "../Physics/src/Calculus/Calculus.lhs"),
    ])
]

build_sections(sources)

build_index(sources)
