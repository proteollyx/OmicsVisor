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

## Licensing review

The licensing position was reviewed at the MDC in September 2026:

- **Research Data Management** confirmed that open publication of research
  software is consistent with the MDC Policy Framework for Research Data
  Management (2021), which includes software and code within its definition of
  research data and points to licences approved by the Open Source Initiative,
  and with the MDC Rules of Good Scientific Practice (2023), which apply the
  FAIR principles to research software and its source code. MIT is
  OSI-approved.
- **Technology Transfer / Innovations** confirmed there is no patentable
  subject matter and no commercial potential, that no third-party material is
  involved, and that publishing under an open licence is therefore fine. They
  also recommended keeping the AI-generated images outside the licence grant,
  as documented above.

Technology Transfer subsequently confirmed the outcome explicitly: publication
under a fully open licence is appropriate, there is no possible future patent and no
third-party material involved, and they saw no reason to restrict commercial use. They
also noted that because the MDC owns the intellectual property, a non-commercial
restriction would not have limited the MDC's own use in any case.

Copyright in software written by MDC staff in the course of their duties rests
with the MDC; the `LICENSE` file names both the author and the institution.

---

## Deployment access

The hosted instance is deliberately **public**: reachable without a login, so
that collaborators outside the MDC, and readers who arrive via the repository or
the DOI, can use it.

This was considered and decided rather than left at a default. The reasoning:

- The application holds no institutional data. It is a viewer over a file the
  user supplies in their own session, and it stores nothing — the upload lives
  in a per-session temporary directory and is discarded with the session.
- The source is public under the MIT licence, so restricting the interface would
  protect nothing that is not already open.
- MDC Technology Transfer confirmed there is no commercial exposure.
- Requiring institutional sign-in would exclude precisely the external
  collaborators the tool exists to serve, and would leave the citable DOI
  pointing at something most readers could not run.

The residual risk is **availability rather than confidentiality**: parsing and
rendering happen server-side, so large uploads consume resources on shared
infrastructure. The appropriate mitigation is per-session resource limits at the
hosting layer, not access control. The application's own upload ceiling is set in
`app.R` (`shiny.maxRequestSize`).

Users are warned in both the startup notice and the Disclaimer tab not to upload
sensitive or personally identifiable data.

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
