# Third-Party Licence Notes

JamoviReport is distributed under GPL-3. The built jamovi module may bundle R
package dependencies needed at runtime. The current compiled dependency audit
identified:

| Package | Bundled version | Licence recorded in bundled `DESCRIPTION` |
| --- | ---: | --- |
| `bayestestR` | 0.16.0 | GPL-3 |
| `datawizard` | 1.1.0 | MIT + file LICENSE |
| `effectsize` | 1.0.0 | MIT + file LICENSE |
| `insight` | 1.3.0 | GPL-3 |
| `parameters` | 0.26.0 | GPL-3 |
| `performance` | 0.14.0 | GPL-3 |

The bundled `datawizard` and `effectsize` directories retain their package
licence files and copyright-holder notices. Those permissive licences are
compatible with distributing the combined module under GPL-3.

This audit covers the current compiled module dependencies only. Repeat it
before each public release if dependencies or generated assets change.

