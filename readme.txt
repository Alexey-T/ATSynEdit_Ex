Package, additional to ATSynEdit. It has the following parts splitted from ATSynEdit, to avoid build errors:

- Adapter for EControl (CudaText/SynWrite) lexers
- Adapter for CudaText "lite" lexers
- Units which implement smart auto-completion:
   - universal auto-completion form with listbox
   - logic for CSS auto-completion
   - logic for HTML auto-completion
   - logic for auto-completion using CudaText's *.acp files (they are shipped with lexers)

Requires packages:

- ATSynEdit
- ATFlatControls
- EControl

Author: Alexey Torgashin (CudaText)
License: MPL 2.0 or LGPL
