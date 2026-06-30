# QuickVTX Installer Wizard — Design Spec

**Date:** 2026-06-30  
**Repo:** [QVTx-Config](https://github.com/Akceptor/QVTx-Config) (GitHub Pages)  
**Scripts source:** [Quick-VTx](https://github.com/Akceptor/Quick-VTx) (this repo)

---

## Overview

Replace the current QVTx-Config `index.html` with a 4-step wizard that installs QuickVTX Lua scripts directly onto a radio's SD card from the browser. No backend. No build step. Single HTML file served via GitHub Pages.

---

## Repository Structure (QVTx-Config)

```
index.html          ← new wizard (replaces current)
advanced.html       ← renamed current index.html
images/
  color-radio.png   ← RadioMaster TX15 photo (placeholder for all color models)
  bw-radio.png      ← RadioMaster Boxer photo (placeholder for all BW models)
```

---

## Architecture

### Delivery

Single self-contained HTML file. All CSS and JS inline. No external dependencies. No build step.

### Script Fetching

Scripts fetched at install time from GitHub raw URLs:

```
https://raw.githubusercontent.com/Akceptor/Quick-VTx/main/QuickVTX%20-%20TX15/SCRIPTS/...
https://raw.githubusercontent.com/Akceptor/Quick-VTx/main/QuickVTX%20-%20BW/SCRIPTS/...
```

Always reflects the latest commit on `main`. Requires internet connection during install.

### SD Card Writing

Uses the **File System Access API** (`showDirectoryPicker` + `getFileHandle({create:true})` + `createWritable()`). Creates intermediate directories as needed. Overwrites existing files only — never deletes or touches directories not in the install list.

**Browser support:** Chrome 86+, Edge 86+ only. Firefox does not support this API. The wizard detects Firefox and shows a blocking warning before the install step.

---

## Files Installed Per Variant

### Color (fetched from `QuickVTX - TX15/SCRIPTS/`)

| Source path | Destination on SD card |
|---|---|
| `SCRIPTS/FUNCTIONS/qvtx.lua` | `SCRIPTS/FUNCTIONS/qvtx.lua` |
| `SCRIPTS/TOOLS/ConfigWizardColor.lua` | `SCRIPTS/TOOLS/ConfigWizardColor.lua` |
| `SCRIPTS/TOOLS/QVTxColor.lua` | `SCRIPTS/TOOLS/QVTxColor.lua` |
| `SCRIPTS/TOOLS/_internal/vtx_auto.lua` | `SCRIPTS/TOOLS/_internal/vtx_auto.lua` |
| `SCRIPTS/TOOLS/_internal/logo.png` | `SCRIPTS/TOOLS/_internal/logo.png` |

### BW/Monochrome (fetched from `QuickVTX - BW/SCRIPTS/`)

| Source path | Destination on SD card |
|---|---|
| `SCRIPTS/TELEMETRY/qvtx.lua` | `SCRIPTS/TELEMETRY/qvtx.lua` |
| `SCRIPTS/TOOLS/ConfigWizardBW.lua` | `SCRIPTS/TOOLS/ConfigWizardBW.lua` |
| `SCRIPTS/TOOLS/QVTxBW.lua` | `SCRIPTS/TOOLS/QVTxBW.lua` |
| `SCRIPTS/TOOLS/_internal/vtx_auto.lua` | `SCRIPTS/TOOLS/_internal/vtx_auto.lua` |

---

## Radio Model Database

Embedded in the HTML as a JS constant. Determines which script variant to install.

### Color screen models

| Brand | Models |
|---|---|
| RadioMaster | TX15, TX16S, TX16S MK3 |
| Jumper | T15, T15 Pro, T16, T18, T22 |

### Monochrome (BW) screen models

| Brand | Models |
|---|---|
| RadioMaster | Boxer, TX12 MK2, Zorro, Pocket, MT12, GX12 |
| Jumper | T-Pro, T-Pro V2, T-Pro S, Bumblebee, T12, T12 MAX, T14, T20, T20 V2 |

---

## Wizard Steps

### Step 1 — Select Radio

- Step indicator at top showing steps 1–4
- Page title: "Select your radio"
- Two sections: RadioMaster | Jumper
- Each section: scrollable grid of model cards
- Card: model name + radio photo (TX15 image for color, Boxer image for BW)
- Clicking a card selects it (accent border + checkmark overlay)
- "Continue" button activates once a model is selected

### Step 2 — Connect Radio

- Title: "Connect your radio"
- Three numbered instructions:
  1. Power on radio
  2. Connect USB cable to computer
  3. On radio screen: select **USB Storage (SD)**
- **Unsupported browser detection:** if `!('showDirectoryPicker' in window)`, show a red banner: _"File installation requires Chrome or Edge."_ — "Select SD Card folder" button remains visible but disabled.
- Primary button: "Select SD Card folder" → calls `window.showDirectoryPicker({mode: 'readwrite'})`
- On success: shows selected folder path as confirmation text, "Continue" activates

### Step 3 — Installing

- Title: "Installing QuickVTX"
- Progress bar (overall %)
- File checklist: each file shown with pending / in-progress / done state
- For each file:
  1. `fetch()` from raw GitHub URL
  2. Navigate/create intermediate directories with `getDirectoryHandle({create:true})`
  3. `getFileHandle(name, {create:true})` then `createWritable()` then `write(blob)` then `close()`
- On any fetch error: show red error with file name and retry option
- On CORS failure: show message "Check your internet connection"
- Auto-advances to Step 4 when all files written successfully

### Step 4 — Done

- Title: "Installation complete!"
- Success icon + confirmation
- Instructions differ by screen type:

**Color screen radios:**
> Open `TOOLS → QVTxColor` on your radio and run the Config Wizard on first use.
>
> To switch channels with a background script:  
> Go to **Special Functions** → pick **Lua Script** function → set **Repeat = ON** → value: `qvtx.lua`

**BW/monochrome radios:**
> Open `TOOLS → QVTxBW` on your radio and run the Config Wizard on first use.
>
> To use the VTx control overlay:  
> Go to **Telemetry screen** settings and assign the **qvtx** script.

---

## Visual Design

Matches the warm-neutral palette from the existing QuickVTX tools:

```css
--bg: #f6f2ea
--panel: #fffaf2
--ink: #1f1b16
--muted: #6b5c4f
--accent: #c24a2e
--accent-dark: #9e3720
--ring: #d9c5ae
--shadow: rgba(20,14,8,0.15)
```

Card-based layout, centered, `min(640px, 100%)` wide. Step indicator is a horizontal pill row at the top. Radio model cards: ~160px wide, image + name, hover lift. Accent color for selected state.

---

## Advanced Button

Fixed position: bottom-right corner. Small, muted styling (not accent). On click: accordion panel slides up from bottom. Initially contains one item:

- **Legacy tools →** links to `advanced.html`

Accordion is a placeholder for future tools — no other items initially.

---

## Error Handling

| Scenario | Behavior |
|---|---|
| `showDirectoryPicker` not available (Firefox / Safari) | Red banner on Step 2, install button disabled |
| `showDirectoryPicker` cancelled | No-op, user stays on Step 2 |
| Fetch fails (network error) | Red inline error on that file, retry button |
| File write fails (permissions) | Red error with message, option to re-select folder |
| SD card looks wrong (no MODELS/ or SCRIPTS/ dir) | Yellow warning: "This might not be the right folder" — but allow proceeding |

---

## Out of Scope

- Offline / no-internet operation (scripts always fetched live)
- Firmware updates
- Model backup or restore
- Any radio not in the Radiomaster/Jumper list
