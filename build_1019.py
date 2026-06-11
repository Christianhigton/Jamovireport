#!/usr/bin/env python3
"""Build jReport_1.0.19.jmo — replaces eduDemographics with redesigned APA module."""

import os, subprocess, sys, zipfile, json, re

JAMOVI_R    = "/Applications/jamovi.app/Contents/Frameworks/R.framework/Versions/4.5-arm64/Resources/bin/R"
JAMOVI_MODS = "/Applications/jamovi.app/Contents/Resources/modules"
BUILD_DIR   = "/Users/christianhigton/Jamovireport/build/R4.5.0-arm64-macos"
PKG_DIR     = "/Users/christianhigton/Jamovireport"
JAMOVI_SRC  = "/Users/christianhigton/Jamovireport/jamovi"
SRC_JMO     = "/Users/christianhigton/Jamovireport/jReport_1.0.18.jmo"
DST_JMO     = "/Users/christianhigton/Jamovireport/jReport_1.0.19.jmo"

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
# Step 2 – build uijs JavaScript for the redesigned eduDemographics
# ---------------------------------------------------------------------------
CUSTOM_ROWS = 5
CUSTOM_FIELDS = [
    ("Char", "Characteristic"),
    ("Val",  "Value"),
    ("Pct",  "%"),
    ("Note", "Note"),
]

options = [
    {"name": "data", "type": "Data"},
    {"name": "tableVariables", "title": "Table Variables", "type": "Variables",
     "suggested": ["nominal", "ordinal", "continuous"],
     "permitted": ["factor", "numeric"], "default": None},
    {"name": "paragraphVariables", "title": "Paragraph Variables", "type": "Variables",
     "suggested": ["nominal", "ordinal", "continuous"],
     "permitted": ["factor", "numeric"], "default": None},
    {"name": "showTable",    "title": "APA Demographics Table", "type": "Bool", "default": True},
    {"name": "showParagraph","title": "APA Paragraph",          "type": "Bool", "default": True},
    {"name": "tableTitle",   "title": "Table Title",            "type": "String",
     "default": "Demographic Characteristics of the Sample"},
    {"name": "showOmitNote", "title": "Omission Note",          "type": "Bool", "default": True},
    {"name": "statMean",        "title": "Mean",    "type": "Bool", "default": True},
    {"name": "statSD",          "title": "SD",      "type": "Bool", "default": True},
    {"name": "statMedian",      "title": "Median",  "type": "Bool", "default": False},
    {"name": "statIQR",         "title": "IQR",     "type": "Bool", "default": False},
    {"name": "statMin",         "title": "Min",     "type": "Bool", "default": False},
    {"name": "statMax",         "title": "Max",     "type": "Bool", "default": False},
    {"name": "statRange",       "title": "Range",   "type": "Bool", "default": False},
    {"name": "statContMissing", "title": "Missing n","type": "Bool", "default": False},
    {"name": "statN",           "title": "n (count)",      "type": "Bool", "default": True},
    {"name": "statPct",         "title": "% (percentage)", "type": "Bool", "default": True},
    {"name": "statCatMissing",  "title": "Missing n",      "type": "Bool", "default": False},
]
for i in range(1, CUSTOM_ROWS + 1):
    for suffix, label in CUSTOM_FIELDS:
        options.append({
            "name":    f"customRow{i}{suffix}",
            "title":   f"Row {i}: {label}",
            "type":    "String",
            "default": ""
        })

options_json = json.dumps(options, separators=(",", ":"))

# Build custom row TextBox controls
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

# Continuous stat CheckBoxes
cont_stats = ["statMean", "statSD", "statMedian", "statIQR",
              "statMin", "statMax", "statRange", "statContMissing"]
cont_checkboxes = ",\n".join(
    f'\t\t\t\t{{\n\t\t\t\t\ttype: DefaultControls.CheckBox,\n'
    f'\t\t\t\t\ttypeName: \'CheckBox\',\n'
    f'\t\t\t\t\tname: "{name}"\n\t\t\t\t}}'
    for name in cont_stats
)

# Categorical stat CheckBoxes
cat_stats = ["statN", "statPct", "statCatMissing"]
cat_checkboxes = ",\n".join(
    f'\t\t\t\t{{\n\t\t\t\t\ttype: DefaultControls.CheckBox,\n'
    f'\t\t\t\t\ttypeName: \'CheckBox\',\n'
    f'\t\t\t\t\tname: "{name}"\n\t\t\t\t}}'
    for name in cat_stats
)

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
    "    stage: 0,\n"
    "    controls: [\n"
    # VariableSupplier with two targets
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.VariableSupplier,\n"
    "\t\t\ttypeName: 'VariableSupplier',\n"
    "\t\t\tstretchFactor: 1,\n"
    "\t\t\tpersistentItems: false,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.TargetLayoutBox,\n"
    "\t\t\t\t\ttypeName: 'TargetLayoutBox',\n"
    "\t\t\t\t\tlabel: \"Table Variables\",\n"
    "\t\t\t\t\tcontrols: [\n"
    "\t\t\t\t\t\t{\n"
    "\t\t\t\t\t\t\ttype: DefaultControls.VariablesListBox,\n"
    "\t\t\t\t\t\t\ttypeName: 'VariablesListBox',\n"
    "\t\t\t\t\t\t\tname: \"tableVariables\",\n"
    "\t\t\t\t\t\t\tisTarget: true\n"
    "\t\t\t\t\t\t}\n"
    "\t\t\t\t\t]\n"
    "\t\t\t\t},\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.TargetLayoutBox,\n"
    "\t\t\t\t\ttypeName: 'TargetLayoutBox',\n"
    "\t\t\t\t\tlabel: \"Paragraph Variables\",\n"
    "\t\t\t\t\tcontrols: [\n"
    "\t\t\t\t\t\t{\n"
    "\t\t\t\t\t\t\ttype: DefaultControls.VariablesListBox,\n"
    "\t\t\t\t\t\t\ttypeName: 'VariablesListBox',\n"
    "\t\t\t\t\t\t\tname: \"paragraphVariables\",\n"
    "\t\t\t\t\t\t\tisTarget: true\n"
    "\t\t\t\t\t\t}\n"
    "\t\t\t\t\t]\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    # Output Options CollapseBox
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Output Options\",\n"
    "\t\t\tcollapsed: false,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.CheckBox,\n"
    "\t\t\t\t\ttypeName: 'CheckBox',\n"
    "\t\t\t\t\tname: \"showTable\"\n"
    "\t\t\t\t},\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.CheckBox,\n"
    "\t\t\t\t\ttypeName: 'CheckBox',\n"
    "\t\t\t\t\tname: \"showParagraph\"\n"
    "\t\t\t\t},\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.TextBox,\n"
    "\t\t\t\t\ttypeName: 'TextBox',\n"
    "\t\t\t\t\tname: \"tableTitle\"\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    # Continuous Statistics CollapseBox
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Continuous Variable Statistics\",\n"
    "\t\t\tcollapsed: false,\n"
    "\t\t\tcontrols: [\n"
    f"{cont_checkboxes}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    # Categorical Statistics CollapseBox
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Categorical Variable Statistics\",\n"
    "\t\t\tcollapsed: false,\n"
    "\t\t\tcontrols: [\n"
    f"{cat_checkboxes}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    # Additional Table Rows CollapseBox
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Additional Table Rows\",\n"
    "\t\t\tcollapsed: true,\n"
    "\t\t\tcontrols: [\n"
    f"{custom_controls_js}\n"
    "\t\t\t]\n"
    "\t\t},\n"
    # Reporting CollapseBox
    "\t\t{\n"
    "\t\t\ttype: DefaultControls.CollapseBox,\n"
    "\t\t\ttypeName: 'CollapseBox',\n"
    "\t\t\tlabel: \"Reporting\",\n"
    "\t\t\tcollapsed: true,\n"
    "\t\t\tcontrols: [\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\ttype: DefaultControls.CheckBox,\n"
    "\t\t\t\t\ttypeName: 'CheckBox',\n"
    "\t\t\t\t\tname: \"showOmitNote\"\n"
    "\t\t\t\t}\n"
    "\t\t\t]\n"
    "\t\t}\n"
    "\t]\n"
    "});\n\n"
    "module.exports = { view : view, options: options };\n\n"
)

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
full_js    = umd_prefix + inner_js + umd_suffix

def encode_yaml_string(s):
    return (s.replace('\\', '\\\\')
             .replace('"', '\\"')
             .replace('\n', '\\n')
             .replace('\t', '\\t'))

uijs_encoded = encode_yaml_string(full_js)
print(f"uijs length: {len(uijs_encoded)} chars")

# ---------------------------------------------------------------------------
# Step 3 – build the jamovi-full.yaml entry for the redesigned eduDemographics
# ---------------------------------------------------------------------------
def option_to_yaml(opt, indent=6):
    pad = " " * indent
    lines = [f"{pad}- name: {opt['name']}"]
    if opt.get("type"):
        lines.append(f"{pad}  type: {opt['type']}")
    if opt.get("title"):
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
      Generate an APA 7 demographic characteristics table and optional
      descriptive paragraph. Supports categorical and continuous variables
      with flexible statistics options and additional custom rows.
    category: analyses
    options:
{options_yaml}
    uijs: "{uijs_encoded}"
"""

# ---------------------------------------------------------------------------
# Step 4 – repack the .jmo, replacing the old eduDemographics entry
# ---------------------------------------------------------------------------
print("Repacking .jmo...")

jreport_build = os.path.join(BUILD_DIR, "jReport")

with zipfile.ZipFile(SRC_JMO, 'r') as zin, \
     zipfile.ZipFile(DST_JMO, 'w', compression=zipfile.ZIP_DEFLATED) as zout:

    for item in zin.infolist():
        name = item.filename

        if name.startswith('jReport/R/jReport/'):
            rel   = name[len('jReport/R/jReport/'):]
            local = os.path.join(jreport_build, rel)
            if not rel or name.endswith('/'):
                zout.writestr(item, b'')
            elif os.path.isfile(local):
                zout.write(local, name)
            else:
                zout.writestr(item, zin.read(name))

        elif name == 'jReport/jamovi-full.yaml':
            content = zin.read(name).decode('utf-8')

            # Replace the existing eduDemographics entry with the new one.
            # The entry starts with "  - title: Demographic Table\n    name: eduDemographics"
            # and ends just before the next top-level entry or usesNative:.
            pattern = re.compile(
                r'  - title: Demographic Table\n    name: eduDemographics\n.*?'
                r'(?=\n  - title:|\nusesNative:)',
                re.DOTALL
            )
            new_content = pattern.sub(demographics_entry.rstrip(), content)
            assert new_content != content, \
                "eduDemographics entry not found in jamovi-full.yaml — cannot replace"

            # Validate: each uijs must be a single line
            lines = new_content.split('\n')
            uijs_lines = [l for l in lines if l.strip().startswith('uijs:')]
            assert len(uijs_lines) == 29, f"Expected 29 uijs entries, got {len(uijs_lines)}"
            assert all(len(l) > 100 for l in uijs_lines), "A uijs entry seems truncated"
            zout.writestr(name, new_content.encode('utf-8'))

        elif name == 'jReport/jamovi.yaml':
            # The jamovi.yaml entry for eduDemographics can stay as-is (title/name only)
            zout.writestr(item, zin.read(name))

        elif name.startswith('jReport/analyses/') and name.endswith(('.a.yaml', '.r.yaml')):
            # Replace with the local version if it exists
            local = os.path.join(JAMOVI_SRC, os.path.basename(name))
            if os.path.isfile(local):
                zout.write(local, name)
            else:
                zout.writestr(item, zin.read(name))

        else:
            zout.writestr(item, zin.read(name))

print(f"\nBuilt: {DST_JMO}")

# ---------------------------------------------------------------------------
# Step 5 – install
# ---------------------------------------------------------------------------
INSTALL_DIR    = "/Users/christianhigton/Library/Application Support/jamovi/modules"
INSTALL_TARGET = os.path.join(INSTALL_DIR, "jReport")
print("Installing...")
subprocess.run(["rm", "-rf", INSTALL_TARGET], check=True)
subprocess.run(
    ["unzip", "-q", "-o", DST_JMO, "-d", INSTALL_TARGET.rsplit("/jReport", 1)[0]],
    check=True
)
print(f"Installed to: {INSTALL_TARGET}")
print("Done!")
