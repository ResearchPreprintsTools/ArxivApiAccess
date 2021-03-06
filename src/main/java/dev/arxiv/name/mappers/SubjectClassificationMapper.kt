package dev.arxiv.name.mappers

import dev.arxiv.name.exeptions.TermNotFoundException

private val classificationMap: Map<String, String> = mapOf(
        Pair("astro-ph", "Astrophysics"),
        Pair("astro-ph.CO", "Cosmology and Nongalactic Astrophysics"),
        Pair("astro-ph.EP", "Earth and Planetary Astrophysics"),
        Pair("astro-ph.GA", "Astrophysics of Galaxies"),
        Pair("astro-ph.HE", "High Energy Astrophysical Phenomena"),
        Pair("astro-ph.IM", "Instrumentation and Methods for Astrophysics"),
        Pair("astro-ph.SR", "Solar and Stellar Astrophysics"),
        Pair("cond-mat.dis-nn", "Disordered Systems and Neural Networks"),
        Pair("cond-mat.mes-hall", "Mesoscale and Nanoscale Physics"),
        Pair("cond-mat.mtrl-sci", "Materials Science"),
        Pair("cond-mat.other", "Other Condensed Matter"),
        Pair("cond-mat.quant-gas", "Quantum Gases"),
        Pair("cond-mat.soft", "Soft Condensed Matter"),
        Pair("cond-mat.stat-mech", "Statistical Mechanics"),
        Pair("cond-mat.str-el", "Strongly Correlated Electrons"),
        Pair("cond-mat.supr-con", "Superconductivity"),
        Pair("cs.AI", "Artificial Intelligence"),
        Pair("cs.AR", "Hardware Architecture"),
        Pair("cs.CC", "Computational Complexity"),
        Pair("cs.CE", "Computational Engineering, Finance, and Science"),
        Pair("cs.CG", "Computational Geometry"),
        Pair("cs.CL", "Computation and Language"),
        Pair("cs.CR", "Cryptography and Security"),
        Pair("cs.CV", "Computer Vision and Pattern Recognition"),
        Pair("cs.CY", "Computers and Society"),
        Pair("cs.DB", "Databases"),
        Pair("cs.DC", "Distributed, Parallel, and Cluster Computing"),
        Pair("cs.DL", "Digital Libraries"),
        Pair("cs.DM", "Discrete Mathematics"),
        Pair("cs.DS", "Data Structures and Algorithms"),
        Pair("cs.ET", "Emerging Technologies"),
        Pair("cs.FL", "Formal Languages and Automata Theory"),
        Pair("cs.GL", "General Literature"),
        Pair("cs.GR", "Graphics"),
        Pair("cs.GT", "Computer Science and Game Theory"),
        Pair("cs.HC", "Human-Computer Interaction"),
        Pair("cs.IR", "Information Retrieval"),
        Pair("cs.IT", "Information Theory"),
        Pair("cs.LG", "Learning"),
        Pair("cs.LO", "Logic in Computer Science"),
        Pair("cs.MA", "Multiagent Systems"),
        Pair("cs.MM", "Multimedia"),
        Pair("cs.MS", "Mathematical Software"),
        Pair("cs.NA", "Numerical Analysis"),
        Pair("cs.NE", "Neural and Evolutionary Computing"),
        Pair("cs.NI", "Networking and Internet Architecture"),
        Pair("cs.OH", "Other Computer Science"),
        Pair("cs.OS", "Operating Systems"),
        Pair("cs.PF", "Performance"),
        Pair("cs.PL", "Programming Languages"),
        Pair("cs.RO", "Robotics"),
        Pair("cs.SC", "Symbolic Computation"),
        Pair("cs.SD", "Sound"),
        Pair("cs.SE", "Software Engineering"),
        Pair("cs.SI", "Social and Information Networks"),
        Pair("cs.SY", "Systems and Control"),
        Pair("econ.EM", "Econometrics"),
        Pair("eess.AS", "Audio and Speech Processing"),
        Pair("eess.IV", "Image and Video Processing"),
        Pair("eess.SP", "Signal Processing"),
        Pair("gr-qc", "General Relativity and Quantum Cosmology"),
        Pair("hep-ex", "High Energy Physics - Experiment"),
        Pair("hep-lat", "High Energy Physics - Lattice"),
        Pair("hep-ph", "High Energy Physics - Phenomenology"),
        Pair("hep-th", "High Energy Physics - Theory"),
        Pair("math.AC", "Commutative Algebra"),
        Pair("math.AG", "Algebraic Geometry"),
        Pair("math.AP", "Analysis of PDEs"),
        Pair("math.AT", "Algebraic Topology"),
        Pair("math.CA", "Classical Analysis and ODEs"),
        Pair("math.CO", "Combinatorics"),
        Pair("math.CT", "Category Theory"),
        Pair("math.CV", "Complex Variables"),
        Pair("math.DG", "Differential Geometry"),
        Pair("math.DS", "Dynamical Systems"),
        Pair("math.FA", "Functional Analysis"),
        Pair("math.GM", "General Mathematics"),
        Pair("math.GN", "General Topology"),
        Pair("math.GR", "Group Theory"),
        Pair("math.GT", "Geometric Topology"),
        Pair("math.HO", "History and Overview"),
        Pair("math.IT", "Information Theory"),
        Pair("math.KT", "K-Theory and Homology"),
        Pair("math.LO", "Logic"),
        Pair("math.MG", "Metric Geometry"),
        Pair("math.MP", "Mathematical Physics"),
        Pair("math.NA", "Numerical Analysis"),
        Pair("math.NT", "Number Theory"),
        Pair("math.OA", "Operator Algebras"),
        Pair("math.OC", "Optimization and Control"),
        Pair("math.PR", "Probability"),
        Pair("math.QA", "Quantum Algebra"),
        Pair("math.RA", "Rings and Algebras"),
        Pair("math.RT", "Representation Theory"),
        Pair("math.SG", "Symplectic Geometry"),
        Pair("math.SP", "Spectral Theory"),
        Pair("math.ST", "Statistics Theory"),
        Pair("math-ph", "Mathematical Physics"),
        Pair("nlin.AO", "Adaptation and Self-Organizing Systems"),
        Pair("nlin.CD", "Chaotic Dynamics"),
        Pair("nlin.CG", "Cellular Automata and Lattice Gases"),
        Pair("nlin.PS", "Pattern Formation and Solitons"),
        Pair("nlin.SI", "Exactly Solvable and Integrable Systems"),
        Pair("nucl-ex", "Nuclear Experiment"),
        Pair("nucl-th", "Nuclear Theory"),
        Pair("physics.acc-ph", "Accelerator Physics"),
        Pair("physics.ao-ph", "Atmospheric and Oceanic Physics"),
        Pair("physics.app-ph", "Applied Physics"),
        Pair("physics.atm-clus", "Atomic and Molecular Clusters"),
        Pair("physics.atom-ph", "Atomic Physics"),
        Pair("physics.bio-ph", "Biological Physics"),
        Pair("physics.chem-ph", "Chemical Physics"),
        Pair("physics.class-ph", "Classical Physics"),
        Pair("physics.comp-ph", "Computational Physics"),
        Pair("physics.data-an", "Data Analysis, Statistics and Probability"),
        Pair("physics.ed-ph", "Physics Education"),
        Pair("physics.flu-dyn", "Fluid Dynamics"),
        Pair("physics.gen-ph", "General Physics"),
        Pair("physics.geo-ph", "Geophysics"),
        Pair("physics.hist-ph", "History and Philosophy of Physics"),
        Pair("physics.ins-det", "Instrumentation and Detectors"),
        Pair("physics.med-ph", "Medical Physics"),
        Pair("physics.optics", "Optics"),
        Pair("physics.plasm-ph", "Plasma Physics"),
        Pair("physics.pop-ph", "Popular Physics"),
        Pair("physics.soc-ph", "Physics and Society"),
        Pair("physics.space-ph", "Space Physics"),
        Pair("q-bio.BM", "Biomolecules"),
        Pair("q-bio.CB", "Cell Behavior"),
        Pair("q-bio.GN", "Genomics"),
        Pair("q-bio.MN", "Molecular Networks"),
        Pair("q-bio.NC", "Neurons and Cognition"),
        Pair("q-bio.OT", "Other Quantitative Biology"),
        Pair("q-bio.PE", "Populations and Evolution"),
        Pair("q-bio.QM", "Quantitative Methods"),
        Pair("q-bio.SC", "Subcellular Processes"),
        Pair("q-bio.TO", "Tissues and Organs"),
        Pair("q-fin.CP", "Computational Finance"),
        Pair("q-fin.EC", "Economics"),
        Pair("q-fin.GN", "General Finance"),
        Pair("q-fin.MF", "Mathematical Finance"),
        Pair("q-fin.PM", "Portfolio Management"),
        Pair("q-fin.PR", "Pricing of Securities"),
        Pair("q-fin.RM", "Risk Management"),
        Pair("q-fin.ST", "Statistical Finance"),
        Pair("q-fin.TR", "Trading and Market Microstructure"),
        Pair("quant-ph", "Quantum Physics"),
        Pair("stat.AP", "Applications"),
        Pair("stat.CO", "Computation"),
        Pair("stat.ME", "Methodology"),
        Pair("stat.ML", "Machine Learning"),
        Pair("stat.OT", "Other Statistics"),
        Pair("stat.TH", "Statistics Theory")
)

/**
 * That function converts the dev.arxiv.name.data.Category#term to a readable form
 */
fun convertTermCode(term: String?): String = convertTermCodeOrNull(term) ?: throw TermNotFoundException("Term $term had not found.")

/**
 * That function converts the dev.arxiv.name.data.Category#term to a readable form
 */
fun convertTermCodeOrDefault(term: String?, defaultValue: String): String = convertTermCodeOrNull(term) ?: defaultValue

/**
 * That function converts the dev.arxiv.name.data.Category#term to a readable form
 */
fun convertTermCodeOrNull(term: String?): String? = term?.let { classificationMap[term] }
