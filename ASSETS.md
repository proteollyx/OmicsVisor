# Asset provenance and licensing

The [MIT licence](LICENSE) covers the **source code** of OmicsVisor. It does
not cover the image assets listed below. This file records where each asset
came from so that the licensing boundary is unambiguous for anyone reusing
this repository.

---

## AI-generated images — not covered by the MIT licence

The following files were generated with **OpenAI ChatGPT**:

| File | Role |
|---|---|
| `www/omics_icon3.png` | Application icon shown in the header |
| `www/hedgehog_1DE.png` | Decorative illustration on the 1D Enrichment tab |

The following files are **derived from `www/omics_icon3.png`** (generated with
a favicon generator) and therefore share its provenance:

| File | Role |
|---|---|
| `www/favicon_io/favicon.ico` | Browser favicon |
| `www/favicon_io/favicon-16x16.png` | Browser favicon, 16 px |
| `www/favicon_io/favicon-32x32.png` | Browser favicon, 32 px |
| `www/favicon_io/apple-touch-icon.png` | iOS home-screen icon |
| `www/favicon_io/android-chrome-192x192.png` | Android icon, 192 px |
| `www/favicon_io/android-chrome-512x512.png` | Android icon, 512 px |

### Status of these files

- They are **not covered by the MIT licence** that applies to the source code.
- **No separate copyright claim is asserted over them.** Purely AI-generated
  images may not be eligible for copyright protection, so no rights are
  claimed and none are granted.
- They are provided **as-is**, purely as decorative interface elements. They
  carry no scientific content and none of the application's functionality
  depends on them.
- If you reuse OmicsVisor and would rather not rely on assets of uncertain
  status, you can delete them and substitute your own. The application
  degrades gracefully: the header simply renders without an icon, and the
  1D Enrichment sidebar without its illustration.

`www/favicon_io/site.webmanifest` is a small generated configuration file, not
an image, and contains no creative content.

---

## Everything else

All source code in this repository — `app.R`, `helper_functions.R`,
`version.R`, the `*_module.R` files and everything under `tests/` — is covered
by the MIT licence. See [LICENSE](LICENSE).

The images above were resized from their originals to match the resolution at
which they are actually displayed; the full-resolution versions remain in this
repository's git history.

For how the code itself was authored, including the use of AI coding
assistants, see the *Development and AI assistance* section of the
[README](README.md).
