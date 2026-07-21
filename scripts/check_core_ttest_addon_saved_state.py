"""Audit whether jamovi's saved analysis payload retains add-on options.

Run with the Python runtime bundled in the target jamovi application. This is
an analysis-payload audit, not a claim that a desktop .omv file was reopened.
"""

import sys

SERVER_ROOT = "/Applications/jamovi.app/Contents/Resources/jamovi/server"
if SERVER_ROOT not in sys.path:
    sys.path.insert(0, SERVER_ROOT)

from jamovi.server import jamovi_pb2 as jcoms
from jamovi.server.analyses.analyses import Analyses


class AnalysisMeta:
    def __init__(self, name, options, addons=None):
        self.name = name
        self.defn = {"options": options}
        self.addons = list(addons or [])

    def translate_defaults(self, module, language):
        return None


class ModuleMeta:
    def __init__(self, analyses):
        self.analyses = analyses

    def get(self, name):
        return self.analyses[name]


class Modules:
    def __init__(self):
        addon_options = [
            {"name": "jreportEnabled", "type": "Bool", "default": False},
            {
                "name": "reportStyle",
                "type": "List",
                "options": [
                    {"name": "apaConcise"},
                    {"name": "apaDetailed"},
                    {"name": "plainLanguage"},
                    {"name": "teaching"},
                ],
                "default": "apaConcise",
            },
        ]
        self.modules = {
            "jmv": ModuleMeta(
                {
                    "ttestIS": AnalysisMeta(
                        "ttestIS",
                        [{"name": "students", "type": "Bool", "default": True}],
                        addons=[("jReport", "jrReportTTestIS")],
                    )
                }
            ),
            "jReport": ModuleMeta(
                {"jrReportTTestIS": AnalysisMeta("jrReportTTestIS", addon_options)}
            ),
        }

    def add_listener(self, listener):
        return None

    def get(self, name):
        return self.modules[name]


def main():
    analyses = Analyses(object(), Modules())
    host = analyses._construct(1, "ttestIS", "jmv")
    addon = host.addons[0]
    addon.options.set_value("jreportEnabled", True)
    addon.options.set_value("reportStyle", "teaching")

    response = jcoms.AnalysisResponse()
    response.analysisId = host.id
    response.name = host.name
    response.ns = host.ns
    response.options.CopyFrom(host.options.as_pb())
    response.status = jcoms.AnalysisStatus.Value("ANALYSIS_COMPLETE")
    response.results.group.SetInParent()
    host.results = response

    saved = jcoms.AnalysisResponse()
    saved.ParseFromString(host.serialize())
    reopened = analyses._construct(3, saved.name, saved.ns, saved.options)
    reopened_addon = reopened.addons[0]

    before = (
        addon.options.get_value("jreportEnabled"),
        addon.options.get_value("reportStyle"),
    )
    after = (
        reopened_addon.options.get_value("jreportEnabled"),
        reopened_addon.options.get_value("reportStyle"),
    )
    saved_names = tuple(saved.options.names)

    print(f"before save: enabled={before[0]}, style={before[1]}")
    print(f"saved host option names: {saved_names}")
    print(f"after reconstruct: enabled={after[0]}, style={after[1]}")
    if before != (True, "teaching"):
        raise SystemExit("The audit fixture did not set the add-on options.")
    if "jreportEnabled" in saved_names or "reportStyle" in saved_names:
        raise SystemExit("Unexpectedly found add-on options in the host payload.")
    if after != (False, "apaConcise"):
        raise SystemExit("Unexpected add-on reconstruction state.")
    print("LIMITATION CONFIRMED: add-on selections are not retained in the host save payload.")


if __name__ == "__main__":
    main()
