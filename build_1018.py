#!/usr/bin/env python3
"""Build jReport_1.0.18.jmo — adds eduDemographics and recompiles R package."""

import os, subprocess, sys, zipfile, json, re

JAMOVI_R   = "/Applications/jamovi.app/Contents/Frameworks/R.framework/Versions/4.5-arm64/Resources/bin/R"
JAMOVI_MODS = "/Applications/jamovi.app/Contents/Resources/modules"
BUILD_DIR  = "/Users/christianhigton/Jamovireport/build/R4.5.0-arm64-macos"
PKG_DIR    = "/Users/christianhigton/Jamovireport"
JAMOVI_SRC = "/Users/christianhigton/Jamovireport/jamovi"
SRC_JMO    = "/Users/christianhigton/Jamovireport/jReport_1.0.17.jmo"
DST_JMO    = "/Users/christianhigton/Jamovireport/jReport_1.0.18.jmo"

# ---------------------------------------------------------------------------
# Step 1 – compile R package
# ---------------------------------------------------------------------------
print("Compiling R package...")
r_cmd = f"""
.libPaths(c('{BUILD_DIR}','{JAMOVI_MODS}/jmv/R','{JAMOVI_MODS}/base/R',.libPaths()))
install.packages('{PKG_DIR}', lib='{BUILD_DIR}', repos=NULL, type='source', quiet=TRUE)
cat('R compile OK\\n')
"""
result = subprocess.run(
    [JAMOVI_R, "--no-save", "--no-restore", "-e", r_cmd],
    capture_output=True, text=True
)
if "R compile OK" not in result.stdout + result.stderr:
    print("STDOUT:", result.stdout[-2000:])
    print("STDERR:", result.stderr[-2000:])
    sys.exit("R compilation failed")
print("R compile OK")

# ---------------------------------------------------------------------------
# Step 2 – build uijs JavaScript for eduDemographics
# ---------------------------------------------------------------------------
CUSTOM_ROWS = 5
CUSTOM_FIELDS = [
    ("Var",  "Variable"),
    ("Cat",  "Category or statistic"),
    ("N",    "n or value"),
    ("Pct",  "%"),
    ("Note", "Note"),
]

# Build options array
options = [
    {"name": "data", "type": "Data"},
    {"name": "variables", "title": "Variables", "type": "Variables",
     "suggested": ["nominal", "ordinal", "continuous"],
     "permitted": ["factor", "numeric"], "default": None},
    {"name": "tableTitle", "title": "Table title", "type": "String",
     "default": "Table 1. Demographic Characteristics of the Sample"},
    {"name": "includeParagraph", "title": "APA descriptive paragraph",
     "type": "Bool", "default": True},
    {"name": "includeCustomInParagraph", "title": "Custom rows in APA paragraph",
     "type": "Bool", "default": False},
]
for i in range(1, CUSTOM_ROWS + 1):
    for suffix, label in CUSTOM_FIELDS:
        options.append({
            "name": f"customRow{i}{suffix}",
            "title": f"Row {i}: {label}",
            "type": "String", "default": ""
        })

options_json = json.dumps(options, separators=(",", ":"))

# Build custom row TextBox controls (25 items)
custom_textboxes = []
for i in range(1, CUSTOM_ROWS + 1):
    for suffix, _ in CUSTOM_FIELDS:
        custom_textboxes.append(
            f'\t\t\t\t{{\n'
            f'\t\t\t\t\ttype: DefaultControls.TextBox,\n'
            f'\t\t\t\t\ttypeName: \'TextBox\',\n'
            f'\t\t\t\t\tname: "customRow{i}{suffix}"\n'
            f'\t\t\t\t}}'
        )

custom_controls_js = ",\n".join(custom_textboxes)

# Main JS code (will be embedded inside browserify wrapper)
inner_js = (
    "\n\n// This file is an automatically generated and should not be edited\n\n"
    "'use strict';\n\n"
    f"const options = {options_json};\n\n"
    "const view = function() {\n"
    "    \n    \n\n"
    "    View.extend({\n"
    "        jus: \"2.0\",\n\n"
    "        events: [\n\n\t]\n\n"
    "    }).call(this);\n"
    "}\n\n"
    "view.layout = ui.extend({\n\n"
    "    label: \"Demographic Table\",\n"
    "    jus: \"2.0\",\n"
    "    type: \"root\",\n"
    "    stage: 0, //0 - release, 1 - development, 2 - proposed\n"
    "    controls: [\n"
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.VariableSupplier,\n"
    "\t\t\ttypeName: 'VariableSupplier',\n"
    "\t\t\tstretchFactor: 1,\n"
    "\t\t\tpersistentItems: false,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.TargetLayoutBox,\n"
    "\t\t\t\t\ttypeName: 'TargetLayoutBox',\n"
    "\t\t\t\t\tlabel: \"Variables (categorical and/or continuous)\",\n"
    "\t\t\t\t\tcontrols: [\n"
    "\t\t\t\t\t\t{\n"
    "\t\t\t\t\t\t\ttype: DefaultControls.VariablesListBox,\n"
    "\t\t\t\t\t\t\ttypeName: 'VariablesListBox',\n"
    "\t\t\t\t\t\t\tname: \"variables\",\n"
    "\t\t\t\t\t\t\tisTarget: true\n"
    "\t\t\t\t\t\t}\n"
    "\t\t\t\t\t]\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Table Options\",\n"
    "\t\t\tcollapsed: false,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.TextBox,\n"
    "\t\t\t\t\ttypeName: 'TextBox',\n"
    "\t\t\t\t\tname: \"tableTitle\"\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Additional Custom Rows\",\n"
    "\t\t\tcollapsed: true,\n"
    "\t\t\tcontrols: [\n"
    f"{custom_controls_js}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Reporting\",\n"
    "\t\t\tcollapsed: true,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.CheckBox,\n"
    "\t\t\t\t\ttypeName: 'CheckBox',\n"
    "\t\t\t\t\tname: \"includeParagraph\"\n"
    "\t\t\t\t},\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.CheckBox,\n"
    "\t\t\t\t\ttypeName: 'CheckBox',\n"
    "\t\t\t\t\tname: \"includeCustomInParagraph\"\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t}\n"
    "\t]\n"
    "});\n\n"
    "module.exports = { view : view, options: options };\n\n"
)

# UMD browserify wrapper
umd_prefix = (
    '(function(f){if(typeof exports==="object"&&typeof module!=="undefined")'
    '{module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}'
    'else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined")'
    '{g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.module = f()}})'
    '(function(){var define,module,exports;return (function(){function r(e,n,t)'
    '{function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;'
    'if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module \'"+i+"\'");'
    'throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,'
    'function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}'
    'for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}'
    'return r})()({1:[function(require,module,exports){'
)
umd_suffix = '\n},{}]},{},[1])(1)\n});\n'

full_js = umd_prefix + inner_js + umd_suffix

def encode_yaml_string(s):
    return (s.replace('\\', '\\\\')
             .replace('"', '\\"')
             .replace('\n', '\\n')
             .replace('\t', '\\t'))

uijs_encoded = encode_yaml_string(full_js)
print(f"uijs length: {len(uijs_encoded)} chars")

# ---------------------------------------------------------------------------
# Step 3 – build the jamovi-full.yaml entry for eduDemographics
# ---------------------------------------------------------------------------
# Build options YAML block from the options list
def option_to_yaml(opt, indent=6):
    pad = " " * indent
    lines = [f"{pad}- name: {opt['name']}"]
    if opt.get("type"):
        lines.append(f"{pad}  type: {opt['type']}")
    if opt.get("title"):
        # Always quote titles — plain scalars may not contain ': ' (YAML mapping indicator)
        t = opt['title'].replace('"', '\\"')
        lines.append(f'{pad}  title: "{t}"')
    if "suggested" in opt:
        lines.append(f"{pad}  suggested:")
        for s in opt["suggested"]:
            lines.append(f"{pad}    - {s}")
    if "permitted" in opt:
        lines.append(f"{pad}  permitted:")
        for p in opt["permitted"]:
            lines.append(f"{pad}    - {p}")
    if "default" in opt:
        v = opt["default"]
        if v is None:
            lines.append(f"{pad}  default: null")
        elif isinstance(v, bool):
            lines.append(f"{pad}  default: {'true' if v else 'false'}")
        elif isinstance(v, str):
            lines.append(f'{pad}  default: "{v}"')
        else:
            lines.append(f"{pad}  default: {v}")
    return "\n".join(lines)

options_yaml = "\n".join(option_to_yaml(o) for o in options)

demographics_entry = f"""  - title: Demographic Table
    name: eduDemographics
    ns: jReport
    menuGroup: jReport
    menuSubgroup: Descriptives
    menuTitle: Demographic Table
    description: >
      Produce an APA-style demographic characteristics table directly copyable
      into Word. Categorical variables show n and % per category; continuous
      variables show M, SD, and range. Custom rows can be added manually.
    category: analyses
    options:
{options_yaml}
    uijs: "{uijs_encoded}"
"""

# ---------------------------------------------------------------------------
# Step 4 – repack the .jmo
# ---------------------------------------------------------------------------
print("Repacking .jmo...")

jreport_build = os.path.join(BUILD_DIR, "jReport")

with zipfile.ZipFile(SRC_JMO, 'r') as zin, \
     zipfile.ZipFile(DST_JMO, 'w', compression=zipfile.ZIP_DEFLATED) as zout:

    for item in zin.infolist():
        name = item.filename

        if name.startswith('jReport/R/jReport/'):
            # Replace with freshly compiled package
            rel = name[len('jReport/R/jReport/'):]
            local = os.path.join(jreport_build, rel)
            if not rel or name.endswith('/'):
                zout.writestr(item, b'')
            elif os.path.isfile(local):
                zout.write(local, name)
            else:
                zout.writestr(item, zin.read(name))

        elif name == 'jReport/jamovi-full.yaml':
            content = zin.read(name).decode('utf-8')
            # Insert demographics entry before 'usesNative:'
            insert_before = 'usesNative: true'
            assert insert_before in content, "Could not find usesNative: true anchor"
            content = content.replace(insert_before, demographics_entry + insert_before, 1)
            # Validate: each uijs must be a single line
            lines = content.split('\n')
            uijs_lines = [l for l in lines if l.strip().startswith('uijs:')]
            assert len(uijs_lines) == 29, f"Expected 29 uijs entries, got {len(uijs_lines)}"
            assert all(len(l) > 100 for l in uijs_lines), "A uijs entry seems truncated"
            zout.writestr(name, content.encode('utf-8'))

        elif name == 'jReport/jamovi.yaml':
            content = zin.read(name).decode('utf-8')
            # Add demographics entry before 'usesNative:'
            dm_yaml = """  - title: Demographic Table
    name: eduDemographics
    ns: jReport
    menuGroup: jReport
    menuSubgroup: Descriptives
    menuTitle: Demographic Table
    description: >
      Produce an APA-style demographic characteristics table directly copyable
      into Word. Categorical variables show n and % per category; continuous
      variables show M, SD, and range. Custom rows can be added manually.
    category: analyses
"""
            content = content.replace('usesNative: true', dm_yaml + 'usesNative: true', 1)
            zout.writestr(name, content.encode('utf-8'))

        elif name.startswith('jReport/analyses/') and name.endswith('.r.yaml'):
            local = os.path.join(JAMOVI_SRC, os.path.basename(name))
            if os.path.isfile(local):
                zout.write(local, name)
            else:
                zout.writestr(item, zin.read(name))

        else:
            zout.writestr(item, zin.read(name))

    # Add new demographics files
    for yaml_name in ('eduDemographics.a.yaml', 'eduDemographics.r.yaml'):
        local = os.path.join(JAMOVI_SRC, yaml_name)
        assert os.path.isfile(local), f"Missing: {local}"
        zout.write(local, f'jReport/analyses/{yaml_name}')

print(f"\nBuilt: {DST_JMO}")

# ---------------------------------------------------------------------------
# Step 5 – install
# ---------------------------------------------------------------------------
INSTALL_DIR = "/Users/christianhigton/Library/Application Support/jamovi/modules"
INSTALL_TARGET = os.path.join(INSTALL_DIR, "jReport")
print("Installing...")
subprocess.run(["rm", "-rf", INSTALL_TARGET], check=True)
subprocess.run(
    ["unzip", "-q", "-o", DST_JMO, "-d", INSTALL_TARGET.rsplit("/jReport", 1)[0]],
    check=True
)
print(f"Installed to: {INSTALL_TARGET}")
print("Done!")
