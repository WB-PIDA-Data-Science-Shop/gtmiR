const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  AlignmentType, HeadingLevel, BorderStyle, WidthType, ShadingType,
  VerticalAlign, PageNumber, PageBreak
} = require('docx');
const fs = require('fs');

// ─── DATA ────────────────────────────────────────────────────────────────────

const GROUPS = [
  {
    name: "Data",
    source: "FORMAL (GTMI .docx table)",
    indicators: [
      { id:"I-2",  label22:"Is there a government enterprise architecture framework?", label25:"Is there a government enterprise architecture framework?", changed:false, in20:true, abbr20:"GEA", note:"Stable. Present all years. Dual-group: also Interoperability." },
      { id:"I-3",  label22:"Is there a government interoperability framework?", label25:"Is there a government interoperability framework?", changed:false, in20:true, abbr20:"GSB", note:"Stable. Present all years. Dual-group: also Interoperability." },
      { id:"I-4",  label22:"Is there a government service bus platform?", label25:"Is there a government service bus platform?", changed:false, in20:false, abbr20:"—", note:"Stable. Introduced in 2022; not tracked in 2020 dataset." },
      { id:"I-29", label22:"Is there an Open Data portal?", label25:"Key indicator", changed:true, in20:false, abbr20:"—", note:"⚠ LABEL CHANGED in 2025 metadata to 'Key indicator' — likely a metadata entry error in 2025 Metadata sheet; substantive question unchanged (confirmed in GTMI_Groups data column). Dual-group: also Digital Engagement." },
      { id:"I-34", label22:"Is there a dedicated government entity in charge of data governance or data management?", label25:"Is there a dedicated government entity in charge of data governance or data management?", changed:false, in20:true, abbr20:"DaG", note:"Stable. Dual-group: also Enablers – Institutions." },
      { id:"I-37", label22:"Are there RTI Laws to make data/info available to the public online or digitally?", label25:"Are there RTI Laws to make data/info available to the public online or digitally?", changed:false, in20:true, abbr20:"RTI", note:"Stable. Dual-group: also Enablers – Data." },
      { id:"I-38", label22:"Is there a Data Protection / Privacy law?", label25:"Is there a Data Protection / Privacy law?", changed:false, in20:true, abbr20:"DPL", note:"Stable. Dual-group: also Enablers – Data." },
      { id:"I-39", label22:"Is there a Data Protection Authority?", label25:"Is there a Data Protection Authority?", changed:false, in20:true, abbr20:"DPA", note:"Stable. Dual-group: also Enablers – Data." },
    ]
  },
  {
    name: "AI",
    source: "FORMAL (GTMI .docx table)",
    indicators: [
      { id:"I-17", label22:"Does government have a national strategy on disruptive / innovative technologies?", label25:"Does government have a national strategy on disruptive / innovative technologies?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020 dataset (introduced 2022). Dual-group: also Innovative Technologies (email)." },
      { id:"I-41", label22:"Are records in the national ID system stored in a digitized (electronic) format?", label25:"Are there ethical guidelines in place for the adoption of Artificial Intelligence?", changed:true, in20:false, abbr20:"—", note:"🔴 CRITICAL RECODE: In 2022 this ID tracked National ID digitization. In 2025 it tracks AI ethical guidelines. These are entirely different constructs — longitudinal analysis for I-41 is NOT valid across 2022→2025 without a data crosswalk." },
    ]
  },
  {
    name: "Institutional Setting",
    source: "FORMAL (GTMI .docx table)",
    indicators: [
      { id:"I-33", label22:"Is there a government entity focused on GovTech (digital transformation, WoG, online services, etc.)?", label25:"Is there a government entity focused on GovTech (digital transformation, WoG, online services, etc.)?", changed:false, in20:true, abbr20:"GT", note:"Stable. 3-year longitudinal feasible. Dual-group: also Enablers – Institutions." },
      { id:"I-35", label22:"Is there a GovTech / Digital Transformation strategy?", label25:"Is there a GovTech / Digital Transformation strategy?", changed:false, in20:true, abbr20:"DGSt", note:"Stable. 3-year longitudinal feasible. Dual-group: also Enablers – Institutions." },
      { id:"I-36", label22:"Is there a whole-of-government approach to public sector digital transformation?", label25:"Is there a whole-of-government approach to public sector digital transformation?", changed:false, in20:true, abbr20:"WoG", note:"Stable. 3-year longitudinal feasible." },
    ]
  },
  {
    name: "Core Government Systems",
    source: "FORMAL (GTMI .docx table)",
    indicators: [
      { id:"I-5",  label22:"Is there an operational FMIS in place to support core PFM functions?", label25:"Is there an operational FMIS in place to support core PFM functions?", changed:false, in20:true, abbr20:"FMIS", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-6",  label22:"Is there a TSA supported by FMIS to automate payments and bank reconciliation?", label25:"Is there a TSA supported by FMIS to automate payments and bank reconciliation?", changed:false, in20:true, abbr20:"TSA", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-7",  label22:"Is there a Tax Management Information System in place?", label25:"Is there a Tax Management Information System in place?", changed:false, in20:true, abbr20:"Tax", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-8",  label22:"Is there a Customs Management Information System in place?", label25:"Is there a Customs Management Information System in place?", changed:false, in20:true, abbr20:"Cust", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-9",  label22:"Is there a Human Resources Management Information System with self-service portal?", label25:"Is there a Human Resources Management Information System with self-service portal?", changed:false, in20:true, abbr20:"HRM", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-10", label22:"Is there a Payroll System (MIS) linked with HRMIS?", label25:"Is there a Payroll System (MIS) linked with HRMIS?", changed:false, in20:true, abbr20:"Payr", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-11", label22:"Is there a Social Insurance system (non-health) providing pensions and other SI programs?", label25:"Is there a Social Insurance system (non-health) providing pensions and other SI programs?", changed:false, in20:false, abbr20:"—", note:"Stable. Not tracked in 2020. 2-year (2022–2025) longitudinal feasible." },
      { id:"I-12", label22:"Is there an e-Procurement portal?", label25:"Is there an e-Procurement portal?", changed:false, in20:true, abbr20:"e-GP", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-13", label22:"Is there a Debt Management System (DMS) in place? (foreign and domestic debt)", label25:"Is there a Debt Management System (DMS) in place? (foreign and domestic debt)", changed:false, in20:true, abbr20:"Debt", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-14", label22:"Is there a Public Investment Management System (PIMS) in place?", label25:"Is there a Public Investment Management System (PIMS) in place?", changed:false, in20:true, abbr20:"PIMS", note:"Stable. 3-year longitudinal feasible." },
      { id:"I-20", label22:"Is there a Tax online service portal?", label25:"Is there a Tax online service portal?", changed:false, in20:true, abbr20:"TaxS", note:"Stable. Dual-group: also Citizen-Facing Services." },
      { id:"I-21", label22:"Is e-Filing available for tax and/or customs declarations?", label25:"Is e-Filing available for tax and/or customs declarations?", changed:false, in20:true, abbr20:"eFil Srv", note:"Stable. Dual-group: also Citizen-Facing Services." },
      { id:"I-22", label22:"Are e-Payment services available?", label25:"Are e-Payment services available?", changed:false, in20:true, abbr20:"ePay Srv", note:"Stable. Dual-group: also Citizen-Facing Services." },
      { id:"I-23", label22:"Is there a Customs online service portal (single window)?", label25:"Is there a Customs online service portal (single window)?", changed:false, in20:true, abbr20:"CusS", note:"Stable. Dual-group: also Citizen-Facing Services." },
      { id:"I-24", label22:"Is there a Social Insurance/Pension online service portal?", label25:"Is there a Social Insurance/Pension online service portal?", changed:false, in20:false, abbr20:"—", note:"Stable. Not tracked in 2020. 2-year longitudinal feasible. Dual-group: also Citizen-Facing Services." },
      { id:"I-25", label22:"Is there a Job portal?", label25:"Is there a Job portal?", changed:false, in20:false, abbr20:"—", note:"Stable. Not tracked in 2020. 2-year longitudinal feasible. Dual-group: also Citizen-Facing Services." },
    ]
  },
  {
    name: "Public Sector Innovation",
    source: "FORMAL (GTMI .docx table)",
    indicators: [
      { id:"I-46", label22:"Is there a strategy and/or program to improve public sector innovation?", label25:"Is there a strategy and/or program to improve public sector innovation?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible. Dual-group: also Enablers – Policies." },
      { id:"I-47", label22:"Is there a government entity focused on public sector innovation?", label25:"Is there a government entity focused on public sector innovation?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible. Dual-group: also Enablers – Institutions." },
      { id:"I-48", label22:"Is there a government policy to support GovTech startups and private sector investments?", label25:"Is there a government policy to support GovTech startups and private sector investments?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible. Dual-group: also Enablers – Policies." },
    ]
  },
  {
    name: "Interoperability (I-1 to I-4)",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-1",  label22:"Is there a shared cloud platform available for all government entities?", label25:"Is there a shared cloud platform available for all government entities?", changed:false, in20:true, abbr20:"GCL", note:"Stable. Note: I-1 is Cloud, not Interoperability per se — Daniel flags I1–I4 as interoperability cluster, but I-1 is better classified as infrastructure enabler." },
      { id:"I-2",  label22:"Is there a government enterprise architecture framework?", label25:"Is there a government enterprise architecture framework?", changed:false, in20:true, abbr20:"GEA", note:"Stable. See also Data group." },
      { id:"I-3",  label22:"Is there a government interoperability framework?", label25:"Is there a government interoperability framework?", changed:false, in20:true, abbr20:"GSB", note:"Stable. See also Data group." },
      { id:"I-4",  label22:"Is there a government service bus platform?", label25:"Is there a government service bus platform?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Data group." },
    ]
  },
  {
    name: "Open Source",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-15", label22:"Is there a government Open Source Software policy/action plan for public sector?", label25:"Is there a government Open Source Software policy/action plan for public sector?", changed:false, in20:true, abbr20:"OSS", note:"Stable. 3-year longitudinal feasible." },
    ]
  },
  {
    name: "Innovative Technologies",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-17", label22:"Does government have a national strategy on disruptive / innovative technologies?", label25:"Does government have a national strategy on disruptive / innovative technologies?", changed:false, in20:false, abbr20:"—", note:"Stable. See also AI group. Propose merging with AI group for formal framework." },
    ]
  },
  {
    name: "Citizen-Facing Services",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-19", label22:"Is there an online public service portal? (also called 'One-Stop Shop' or similar)", label25:"Is there an online public service portal? (also called 'One-Stop Shop' or similar)", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
      { id:"I-20", label22:"Is there a Tax online service portal?", label25:"Is there a Tax online service portal?", changed:false, in20:true, abbr20:"TaxS", note:"Stable. See also Core Gov Systems." },
      { id:"I-21", label22:"Is e-Filing available for tax and/or customs declarations?", label25:"Is e-Filing available for tax and/or customs declarations?", changed:false, in20:true, abbr20:"eFil Srv", note:"Stable. See also Core Gov Systems." },
      { id:"I-22", label22:"Are e-Payment services available?", label25:"Are e-Payment services available?", changed:false, in20:true, abbr20:"ePay Srv", note:"Stable. See also Core Gov Systems." },
      { id:"I-23", label22:"Is there a Customs online service portal (single window)?", label25:"Is there a Customs online service portal (single window)?", changed:false, in20:true, abbr20:"CusS", note:"Stable. See also Core Gov Systems." },
      { id:"I-24", label22:"Is there a Social Insurance/Pension online service portal?", label25:"Is there a Social Insurance/Pension online service portal?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Core Gov Systems." },
      { id:"I-25", label22:"Is there a Job portal?", label25:"Is there a Job portal?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Core Gov Systems." },
      { id:"I-26", label22:"Is there a digital ID [credential/system] that enables remote authentication for (fully) online service access?", label25:"Is there a Digital ID (or equivalent) used for identification and online services based on a unique national ID?", changed:true, in20:false, abbr20:"—", note:"⚠ LABEL CHANGED (rewording, not recode): Both versions track digital ID for service access; the 2025 wording shifts emphasis to foundational ID linkage. Assess sub-indicator continuity before pooling." },
      { id:"I-42", label22:"Is there a digital signature regulation and PKI to support service delivery?", label25:"Is there a digital signature regulation and PKI to support service delivery?", changed:false, in20:false, abbr20:"—", note:"Stable. Proposed addition by Daniel/Joao. Not in 2020. 2-year longitudinal feasible." },
    ]
  },
  {
    name: "Digital Engagement with Citizens",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-28", label22:"Is there an Open Government portal?", label25:"Is there an Open Government portal?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
      { id:"I-29", label22:"Is there an Open Data portal?", label25:"Key indicator [metadata entry issue — see note]", changed:true, in20:false, abbr20:"—", note:"⚠ Metadata label in 2025 sheet reads 'Key indicator' — likely a formatting artifact. Data column I-29 in GTMI_Groups is present and consistent. Verify 2025 Metadata sheet before publication." },
      { id:"I-30", label22:"Are there national platforms that allow citizens to participate in policy decision-making?", label25:"Are there national platforms that allow citizens to participate in policy decision-making?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
      { id:"I-31", label22:"Are there government platforms that allow citizens to provide feedback on service delivery?", label25:"Are there government platforms that allow citizens to provide feedback on service delivery?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
      { id:"I-32", label22:"Does the government publish its citizen engagement statistics and performance regularly?", label25:"Does the government publish its citizen engagement statistics and performance regularly?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
    ]
  },
  {
    name: "Enablers – Data",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-34", label22:"Is there a dedicated government entity in charge of data governance or data management?", label25:"Is there a dedicated government entity in charge of data governance or data management?", changed:false, in20:true, abbr20:"DaG", note:"Stable. See also Data group." },
      { id:"I-37", label22:"Are there RTI Laws to make data/info available to the public online or digitally?", label25:"Are there RTI Laws to make data/info available to the public online or digitally?", changed:false, in20:true, abbr20:"RTI", note:"Stable. See also Data group." },
      { id:"I-38", label22:"Is there a Data Protection / Privacy law?", label25:"Is there a Data Protection / Privacy law?", changed:false, in20:true, abbr20:"DPL", note:"Stable. See also Data group." },
      { id:"I-39", label22:"Is there a Data Protection Authority?", label25:"Is there a Data Protection Authority?", changed:false, in20:true, abbr20:"DPA", note:"Stable. See also Data group." },
    ]
  },
  {
    name: "Enablers – Institutions",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-33", label22:"Is there a government entity focused on GovTech (digital transformation, WoG, online services, etc.)?", label25:"Is there a government entity focused on GovTech (digital transformation, WoG, online services, etc.)?", changed:false, in20:true, abbr20:"GT", note:"Stable. See also Institutional Setting." },
      { id:"I-35", label22:"Is there a GovTech / Digital Transformation strategy?", label25:"Is there a GovTech / Digital Transformation strategy?", changed:false, in20:true, abbr20:"DGSt", note:"Stable. See also Institutional Setting." },
      { id:"I-47", label22:"Is there a government entity focused on public sector innovation?", label25:"Is there a government entity focused on public sector innovation?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Public Sector Innovation." },
    ]
  },
  {
    name: "Enablers – Policies",
    source: "EMAIL (Daniel's grouping proposal)",
    indicators: [
      { id:"I-40", label22:"Is there a national ID (or similar foundational ID) system?", label25:"Is there a government strategy or policy in place to promote the integration of Green Technology within GovTech initiatives?", changed:true, in20:false, abbr20:"—", note:"🔴 CRITICAL RECODE: In 2022, I-40 tracked National ID systems. In 2025 it tracks GreenTech/GovTech policy. Longitudinal analysis across years INVALID — must treat as two separate variables. Daniel's email references I-40 under Policies (intent: policy enabler). The 2025 GreenTech meaning aligns with that intent; the 2022 National ID meaning does not." },
      { id:"I-41", label22:"Are records in the national ID system stored in a digitized (electronic) format?", label25:"Are there ethical guidelines in place for the adoption of Artificial Intelligence?", changed:true, in20:false, abbr20:"—", note:"🔴 CRITICAL RECODE: See AI group. In 2025, this ID covers AI ethics (correct for Policies/AI group). In 2022, it covered National ID digitization. Not longitudinally comparable." },
      { id:"I-45", label22:"Is there a government strategy / program to improve digital skills in the public sector?", label25:"Is there a government strategy / program to improve digital skills in the public sector?", changed:false, in20:false, abbr20:"—", note:"Stable. Not in 2020. 2-year longitudinal feasible." },
      { id:"I-46", label22:"Is there a strategy and/or program to improve public sector innovation?", label25:"Is there a strategy and/or program to improve public sector innovation?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Public Sector Innovation." },
      { id:"I-48", label22:"Is there a government policy to support GovTech startups and private sector investments?", label25:"Is there a government policy to support GovTech startups and private sector investments?", changed:false, in20:false, abbr20:"—", note:"Stable. See also Public Sector Innovation." },
    ]
  },
];

// Also add orphan indicators not in docx
const ORPHANS = [
  { id:"I-16", label22:"UN Telecommunication Infrastructure Index (TII)", label25:"UN Telecommunication Infrastructure Index (TII)", changed:false, in20:false, abbr20:"eTII", note:"ORPHAN: Present in both Excel datasets. Not referenced in .docx or Daniel's email. External index — not a GTMI survey question." },
  { id:"I-18", label22:"UN Online Service Index (OSI)", label25:"UN Online Service Index (OSI)", changed:false, in20:false, abbr20:"eOSI", note:"ORPHAN: Present in both Excel datasets. Not referenced in .docx or Daniel's email. External index — not a GTMI survey question." },
  { id:"I-27", label22:"Is there a cyber security strategy?", label25:"Is there a cyber security strategy?", changed:false, in20:false, abbr20:"—", note:"ORPHAN: Present in both Excel datasets. Not formally grouped in .docx. Daniel's email notes this may be from external indexes — warrants team decision on inclusion." },
  { id:"I-43", label22:"ITU Global Cybersecurity Index (GCI)", label25:"ITU Global Cybersecurity Index (GCI)", changed:false, in20:false, abbr20:"—", note:"ORPHAN: Present in both Excel datasets. Not referenced in .docx. External index — not a GTMI survey question." },
  { id:"I-44", label22:"UN Human Capital Index (HCI)", label25:"UN Human Capital Index (HCI)", changed:false, in20:false, abbr20:"—", note:"ORPHAN: Present in both Excel datasets. Not referenced in .docx. External index — not a GTMI survey question." },
];

// ─── STYLES ──────────────────────────────────────────────────────────────────

const COLORS = {
  darkBlue:   "1F3864",
  midBlue:    "2E74B5",
  lightBlue:  "D6E4F0",
  stable:     "E8F5E9",  // green-ish
  changed:    "FFF3CD",  // amber
  critical:   "FDECEA",  // red-ish
  orphan:     "F3E5F5",  // purple-ish
  headerText: "FFFFFF",
  formalGrp:  "1F3864",
  emailGrp:   "1A5276",
  border:     "AAAAAA",
};

function border(color) {
  return { style: BorderStyle.SINGLE, size: 1, color: color || COLORS.border };
}
const BORDERS = { top: border(), bottom: border(), left: border(), right: border() };

function cell(text, opts = {}) {
  const {
    bold = false, italic = false, size = 18, color = "000000",
    bg = "FFFFFF", width, shade = ShadingType.CLEAR, valign, colspan,
    align = AlignmentType.LEFT, wrap = true
  } = opts;
  return new TableCell({
    width: width ? { size: width, type: WidthType.DXA } : undefined,
    columnSpan: colspan,
    verticalAlign: valign || VerticalAlign.TOP,
    shading: { fill: bg, type: shade },
    borders: BORDERS,
    margins: { top: 80, bottom: 80, left: 120, right: 120 },
    children: [
      new Paragraph({
        alignment: align,
        children: [new TextRun({ text, bold, italic, size, color, font: "Arial" })]
      })
    ]
  });
}

function headerRow(cols) {
  return new TableRow({
    tableHeader: true,
    children: cols.map(c =>
      new TableCell({
        width: c.width ? { size: c.width, type: WidthType.DXA } : undefined,
        shading: { fill: c.bg || COLORS.darkBlue, type: ShadingType.CLEAR },
        borders: BORDERS,
        margins: { top: 100, bottom: 100, left: 120, right: 120 },
        children: [new Paragraph({
          alignment: AlignmentType.CENTER,
          children: [new TextRun({ text: c.text, bold: true, size: 18, color: "FFFFFF", font: "Arial" })]
        })]
      })
    )
  });
}

// Column widths (DXA) — total = 9360 (US letter, 1" margins)
const COL_W = [630, 1200, 2200, 2200, 480, 480, 480, 1690];
// ID | Label 2022 | Label 2025 | Changed | 2020 | 2020 Abbr | Feasibility | Notes/Flags

function rowBg(ind) {
  if (ind.note.startsWith("🔴")) return COLORS.critical;
  if (ind.note.startsWith("⚠")) return COLORS.changed;
  return COLORS.stable;
}

function feasibility(ind) {
  if (ind.note.startsWith("🔴")) return "❌ Blocked";
  if (!ind.in20) return "✓ 2yr";
  return "✓ 3yr";
}

function buildIndicatorRow(ind) {
  const bg = rowBg(ind);
  return new TableRow({
    children: [
      cell(ind.id,     { bg, bold:true, size:17, width: COL_W[0] }),
      cell(ind.label22.length > 120 ? ind.label22.substring(0,120)+"…" : ind.label22, { bg, size:16, width: COL_W[1] }),
      cell(ind.label25.length > 120 ? ind.label25.substring(0,120)+"…" : ind.label25, { bg, size:16, width: COL_W[2] }),
      cell(ind.changed ? "YES" : "No",  { bg: ind.changed ? COLORS.changed : bg, bold: ind.changed, size:17, align: AlignmentType.CENTER, width: COL_W[3] }),
      cell(ind.in20 ? "✓" : "—",       { bg, size:17, align: AlignmentType.CENTER, width: COL_W[4] }),
      cell(ind.abbr20,                  { bg, size:16, align: AlignmentType.CENTER, width: COL_W[5] }),
      cell(feasibility(ind),            { bg, size:16, align: AlignmentType.CENTER, width: COL_W[6] }),
      cell(ind.note,                    { bg, size:15, italic:true, width: COL_W[7] }),
    ]
  });
}

function groupSectionTable(group) {
  const isEmail = group.source.startsWith("EMAIL");
  const grpBg = isEmail ? COLORS.emailGrp : COLORS.formalGrp;

  const rows = [
    // Group header spanning all cols
    new TableRow({
      children: [new TableCell({
        columnSpan: 8,
        shading: { fill: grpBg, type: ShadingType.CLEAR },
        borders: BORDERS,
        margins: { top: 100, bottom: 100, left: 160, right: 120 },
        children: [new Paragraph({
          children: [
            new TextRun({ text: group.name.toUpperCase(), bold: true, size: 22, color: "FFFFFF", font: "Arial" }),
            new TextRun({ text: `   (${group.source})`, bold: false, size: 18, color: "DDDDDD", font: "Arial" }),
          ]
        })]
      })]
    }),
    // Column headers
    headerRow([
      { text: "ID",          width: COL_W[0], bg: COLORS.midBlue },
      { text: "Label (2022 / Oct2022 Dataset)", width: COL_W[1], bg: COLORS.midBlue },
      { text: "Label (2025 / Dec2025 Dataset)", width: COL_W[2], bg: COLORS.midBlue },
      { text: "Label Changed?", width: COL_W[3], bg: COLORS.midBlue },
      { text: "In 2020?",    width: COL_W[4], bg: COLORS.midBlue },
      { text: "2020 Abbr",  width: COL_W[5], bg: COLORS.midBlue },
      { text: "Longitudinal Feasibility", width: COL_W[6], bg: COLORS.midBlue },
      { text: "Coherence Notes & Flags",  width: COL_W[7], bg: COLORS.midBlue },
    ]),
    ...group.indicators.map(buildIndicatorRow),
  ];

  return new Table({
    width: { size: 9360, type: WidthType.DXA },
    columnWidths: COL_W,
    rows,
  });
}

// ─── DOCUMENT ────────────────────────────────────────────────────────────────

function p(text, opts = {}) {
  const { bold = false, size = 22, color = "000000", spaceBefore = 80, spaceAfter = 80, align } = opts;
  return new Paragraph({
    alignment: align,
    spacing: { before: spaceBefore, after: spaceAfter },
    children: [new TextRun({ text, bold, size, color, font: "Arial" })]
  });
}

function legend() {
  const items = [
    { bg: COLORS.stable,   text: "✓ 3yr — Stable label, present in all three datasets (2020, 2022, 2025). Full longitudinal analysis feasible." },
    { bg: COLORS.stable,   text: "✓ 2yr — Stable label, present in 2022 and 2025 only. 2-year longitudinal analysis feasible." },
    { bg: COLORS.changed,  text: "⚠  — Label rewording detected between 2022 and 2025. Verify sub-indicator continuity before pooling data." },
    { bg: COLORS.critical, text: "🔴 CRITICAL RECODE — The indicator ID has been reassigned to a completely different construct. Cross-year analysis INVALID without explicit crosswalk." },
    { bg: COLORS.orphan,   text: "ORPHAN — Present in Excel datasets but not referenced in the master .docx groupings. Requires team decision." },
  ];
  return new Table({
    width: { size: 9360, type: WidthType.DXA },
    columnWidths: [300, 9060],
    rows: [
      new TableRow({ children: [
        new TableCell({ columnSpan: 2, borders: BORDERS, shading: { fill: COLORS.darkBlue, type: ShadingType.CLEAR }, margins: { top:80, bottom:80, left:120, right:120 },
          children: [new Paragraph({ children: [new TextRun({ text: "LEGEND — ROW COLOR CODING & FEASIBILITY FLAGS", bold:true, size:20, color:"FFFFFF", font:"Arial" })] })]
        })
      ]}),
      ...items.map(it => new TableRow({ children: [
        new TableCell({ width:{ size:300, type:WidthType.DXA }, shading:{ fill: it.bg, type: ShadingType.CLEAR }, borders: BORDERS, margins:{top:60,bottom:60,left:80,right:80}, children:[new Paragraph({ children:[new TextRun({ text:"", size:18 })] })] }),
        new TableCell({ borders: BORDERS, margins:{top:60,bottom:60,left:120,right:120}, children:[new Paragraph({ children:[new TextRun({ text: it.text, size:17, font:"Arial" })] })] }),
      ]}))
    ]
  });
}

function orphanTable() {
  return new Table({
    width: { size: 9360, type: WidthType.DXA },
    columnWidths: COL_W,
    rows: [
      new TableRow({ children: [new TableCell({ columnSpan:8, shading:{ fill:"6A1B9A", type:ShadingType.CLEAR }, borders: BORDERS, margins:{top:100,bottom:100,left:160,right:120},
        children:[new Paragraph({ children:[
          new TextRun({ text:"ORPHAN CODES", bold:true, size:22, color:"FFFFFF", font:"Arial" }),
          new TextRun({ text:"   (In Excel datasets but NOT referenced in .docx or email groupings)", size:18, color:"DDDDDD", font:"Arial" }),
        ]})]
      })] }),
      headerRow([
        { text:"ID", width:COL_W[0], bg:"6A1B9A" },
        { text:"Label (2022)", width:COL_W[1], bg:"6A1B9A" },
        { text:"Label (2025)", width:COL_W[2], bg:"6A1B9A" },
        { text:"Label Changed?", width:COL_W[3], bg:"6A1B9A" },
        { text:"In 2020?", width:COL_W[4], bg:"6A1B9A" },
        { text:"2020 Abbr", width:COL_W[5], bg:"6A1B9A" },
        { text:"Longitudinal Feasibility", width:COL_W[6], bg:"6A1B9A" },
        { text:"Notes", width:COL_W[7], bg:"6A1B9A" },
      ]),
      ...ORPHANS.map(ind => new TableRow({ children: [
        cell(ind.id, { bg:COLORS.orphan, bold:true, size:17, width:COL_W[0] }),
        cell(ind.label22, { bg:COLORS.orphan, size:16, width:COL_W[1] }),
        cell(ind.label25, { bg:COLORS.orphan, size:16, width:COL_W[2] }),
        cell(ind.changed?"YES":"No", { bg:COLORS.orphan, size:17, align:AlignmentType.CENTER, width:COL_W[3] }),
        cell(ind.in20?"✓":"—", { bg:COLORS.orphan, size:17, align:AlignmentType.CENTER, width:COL_W[4] }),
        cell(ind.abbr20, { bg:COLORS.orphan, size:16, align:AlignmentType.CENTER, width:COL_W[5] }),
        cell(feasibility(ind), { bg:COLORS.orphan, size:16, align:AlignmentType.CENTER, width:COL_W[6] }),
        cell(ind.note, { bg:COLORS.orphan, size:15, italic:true, width:COL_W[7] }),
      ]}))
    ]
  });
}

const children = [
  // Title block
  p("GTMI – Transversal Coherence & Longitudinal Integration Report", { bold:true, size:32, color:COLORS.darkBlue, spaceBefore:0, spaceAfter:80 }),
  p("WBG GovTech Metadata Index (GTMI) | Data Scientist Evaluation", { size:22, color:"555555", spaceAfter:60 }),
  p("Sources: GTMI – Groups of Indicators (Draft).docx  ·  WBG_GovTech_Dataset_Dec2025.xlsx  ·  WBG_GovTech_Dataset_Oct2022.xlsx  ·  wbg_dgssdataset_december2020.xlsx", { size:18, color:"666666", spaceAfter:120 }),
  p("PHASE I – SCHEMA ALIGNMENT SUMMARY", { bold:true, size:24, color:COLORS.darkBlue, spaceBefore:160, spaceAfter:80 }),
  p("• 48 top-level indicator IDs (I-1 to I-48) are present in both the 2022 and 2025 Excel Metadata and data sheets — perfect structural symmetry between these two years.", { size:20, spaceAfter:60 }),
  p("• 43 of those 48 IDs are referenced in the master .docx groupings (formal tables + Daniel's email proposals).", { size:20, spaceAfter:60 }),
  p("• 0 Ghost Codes: every ID in the .docx is confirmed present in both Excel datasets. No phantom references.", { size:20, spaceAfter:60 }),
  p("• 5 Orphan Codes (I-16, I-18, I-27, I-43, I-44): present in Excel but absent from the .docx. All are external indexes, not GTMI survey questions. Decision required: include as contextual variables or exclude from grouped analysis.", { size:20, spaceAfter:60 }),
  p("• 4 Semantic Changes detected between 2022→2025 Metadata: I-26 (wording), I-29 (metadata artifact), I-40 (CRITICAL recode), I-41 (CRITICAL recode).", { size:20, spaceAfter:60 }),
  p("• The 2020 dataset uses abbreviated column labels (e.g. GCL, FMIS, DPL) rather than I-N codes. Crosswalk feasible for 24 of 43 referenced indicators.", { size:20, spaceAfter:120 }),
  p("PHASE II – LONGITUDINAL FEASIBILITY SUMMARY", { bold:true, size:24, color:COLORS.darkBlue, spaceBefore:120, spaceAfter:80 }),
  p("• 3-year panel (2020–2022–2025): 24 indicators. Stable labels + present in all three datasets.", { size:20, spaceAfter:60 }),
  p("• 2-year panel (2022–2025 only): 17 indicators. Stable labels, introduced 2022 or later.", { size:20, spaceAfter:60 }),
  p("• Blocked (cross-year invalid): 2 indicators. I-40 and I-41 were recoded to entirely different constructs in 2025. Their IDs must be treated as two separate variables in any longitudinal model.", { size:20, spaceAfter:120 }),
  legend(),
  p("", { spaceAfter:200 }),
  p("SECTION A — FORMAL GROUPS (from .docx Tables)", { bold:true, size:26, color:COLORS.darkBlue, spaceBefore:160, spaceAfter:100 }),
];

for (const group of GROUPS.filter(g => g.source.startsWith("FORMAL"))) {
  children.push(groupSectionTable(group));
  children.push(p("", { spaceAfter:160 }));
}

children.push(p("SECTION B — WORKING GROUPS (from Daniel's Email Proposals)", { bold:true, size:26, color:COLORS.darkBlue, spaceBefore:200, spaceAfter:100 }));
children.push(p("Note: Several indicators appear in both Section A and Section B (dual membership). This is intentional and reflects the draft nature of the groupings.", { size:20, color:"555555", spaceAfter:100 }));

for (const group of GROUPS.filter(g => g.source.startsWith("EMAIL"))) {
  children.push(groupSectionTable(group));
  children.push(p("", { spaceAfter:160 }));
}

children.push(p("SECTION C — ORPHAN CODES (In Excel, Not Referenced in .docx)", { bold:true, size:26, color:"6A1B9A", spaceBefore:200, spaceAfter:100 }));
children.push(orphanTable());

children.push(p("", { spaceAfter:160 }));
children.push(p("ANALYST RECOMMENDATIONS", { bold:true, size:24, color:COLORS.darkBlue, spaceBefore:200, spaceAfter:100 }));
const recs = [
  "1. CRITICAL — Resolve I-40 and I-41 recodes before any pooled analysis. The 2022 values for these IDs (National ID system, National ID digitization) are not comparable to the 2025 values (GreenTech policy, AI ethics). Create separate named variables (e.g., I-40_NatID_2022, I-40_GreenTech_2025).",
  "2. HIGH — Verify the I-29 Metadata label in the Dec2025 file. The entry 'Key indicator' appears to be a data entry error in the Metadata sheet; the GTMI_Groups data column is correctly populated. Fix before external release.",
  "3. MEDIUM — Review I-26 label evolution (Digital ID rewording). Sub-indicators changed between years; check response-scale continuity before treating as a single longitudinal series.",
  "4. MEDIUM — Decide on Orphan Codes (I-16, I-18, I-27, I-43, I-44). These external indexes are present in all Excel datasets but not mapped to any .docx group. If retained as contextual/control variables, document them explicitly in the codebook.",
  "5. LOW — Resolve group membership overlaps. Several indicators (e.g., I-2, I-3, I-20–I-25, I-29, I-33–I-35) appear in multiple groups. Decide whether to maintain dual membership with weighting rules or assign exclusive groups for index construction.",
  "6. LOW — For the 2020 dataset, build a formal crosswalk table mapping abbreviated column headers (GCL, FMIS, DPL …) to I-N codes, to support automated 3-year panel construction.",
];
for (const rec of recs) {
  children.push(p(rec, { size:19, spaceAfter:80 }));
}

const doc = new Document({
  styles: {
    default: { document: { run: { font:"Arial", size:22 } } }
  },
  sections: [{
    properties: {
      page: {
        size: { width: 15840, height: 12240 },
        margin: { top: 720, right: 720, bottom: 720, left: 720 }
      }
    },
    children,
  }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/home/claude/GTMI_Coherence_Report.docx', buf);
  console.log('Done.');
});
