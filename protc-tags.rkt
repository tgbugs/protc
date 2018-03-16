(tag-doc 'ILXT:ACSF 'ilxtr:identifier
         "ACSF solution, usually the full recipe should be annotated.")
(tag-doc 'ILXT:cs-internal-solution 'ilxtr:identifier
         "Caesium (Cs) based internal solutions, usually the full recipe should be annotated.")
(tag-doc 'ILXT:cut-buffer 'ilxtr:identifier
         "Cutting or slicing solution, usually the full recipe should be annotated.")
(tag-doc 'ILXT:internal-solution 'ilxtr:identifier
         "Internal solutions and their formulas, usually the full recipe should be annotated.")
(tag-doc 'NIFORG:birnlex_254 'ilxtr:identifier
         "Input, [wistar rats?]")
(tag-doc 'RRID:SCR_001775 'ilxtr:identifier
         "Input, Neurolucida system (MicroBrightField Inc., USA)")
(tag-doc 'RRID:SCR_002526 'ilxtr:identifier
         "Steps that have stereo investigator software (7.0 MicroBright Field) as an input")
(tag-doc 'RRID:SCR_007370 'ilxtr:identifier
         "Input, Imaris scientific visualization (Bitplane)")
(tag-doc 'RRID:SCR_014199 'ilxtr:identifier
         "Input, Adobe Photoshop CS4 software (Adobe Systems, San Jose, CA)")
(tag-doc 'RRIDCUR:Missing 'ilxtr:identifier
         "Tried to find the `protc:input` but could not and need to bug the authors")
(tag-doc 'TODO 'nil
         "A note to come back to this block and work on it more thoroughly")
(tag-doc 'mo:analysis 'ilxtr:technique
         "a large chunk of text that talks about how data were analyized (as opposed to collected)")
(tag-doc 'mo:electrophysiology 'ilxtr:technique
         "Analysis or capture of electrical recordings.")
(tag-doc 'mo:extracellular-electrophysiology 'ilxtr:technique
         "Classification of a protocol/set of protocols/steps based on the fact that it records electrical potentials using an extracellular probe.")
(tag-doc 'mo:histology 'ilxtr:technique
         "Steps that involve preparing tissue for study")
(tag-doc 'mo:immunohistochemistry 'ilxtr:technique
         "Protocols that have all the components to classify them as immunohistochemistry")
(tag-doc 'mo:method 'ilxtr:technique
         "High level approaches to making measurement or creating an input, sum of the inputs and techniques.")
(tag-doc 'mo:protocol 'ilxtr:protocol
         "A protocol has been explicitly mentioned in a paper")
(tag-doc 'mo:specification '(ilxtr:deprecatedTag)
         "Detailed description involving the desired outcome, probably should be `protc:objective*`.")
(tag-doc 'mo:technique 'ilxtr:technique
         "Simple, concise verbs describing procedure (that you can learn)")
(tag-doc 'protc:*make* '(ilxtr:deprecatedTag protc:output)
         "Descriptions of how to set up the lab environment (microscope, etc) or a specific input. See also `protc:output`, `protc:input` [3-15-18 This is an old usage, should be replaced by protc:output and protc:input if it is also used within the protocol (it usually is).]")
(tag-doc 'protc:*measure '(ilxtr:measure ilxtr:aspect)
         "Takes `protc:input`s. Returns a `protc:symbolic-output`. The name of the subset of the world that we are interested in producing a number about?? [stick to the function name version of this?] [3-15-18: The actual text of most of these are aspects. From the text it is clear that they are aspects that are intended to be measured. The correct way to model these is probably with protc:aspect + protc:*measure] ")
(tag-doc 'protc:ambiguous-error 'protc:error
         "error: steps are not spelled out clearly enough or may be confusing with previous information, this indicates that we as curators are explicitly marking where we will interpret no further")
(tag-doc 'protc:aspect  'ilxtr:aspect
         "Descriptions involving the current state (size, location, composition), [these could almost be black box components]")
(tag-doc 'protc:black-box 'ilxtr:participant
         "The whole 'containing' spatial and temporal scope for part of a protocol, e.g. a mouse from P0 to P14 (what’s the difference between all the black boxes?)")
(tag-doc 'protc:black-box-component 'ilxtr:participant
         "Parts of the system being studied (brain region, landmark, membranes, neurons, solutions, 4d phenomena). Something that would be labeled in a `protc:delegated-image` to help the executor understand later textual explainations.")
(tag-doc 'protc:black-box-spec 'ilxtr:participant
         "See `protc:aspect`, `protc:black-box-component`, for spec vs component the spec could be considered as context, or an invariant in the context of this measurement.")
(tag-doc 'protc:delegated-image 'ilxtr:executorSemantics
         "Steps that reference a schematic or drawing, these may be labeled and return names")
(tag-doc 'protc:delegated-instructions 'ilxtr:executorSemantics
         "Similar to `protc:executor-verb` but usually operating at a higher level, instructions given without enough background or context since the executor is expected to know what they mean")
(tag-doc 'protc:executor-verb 'ilxtr:executorSemantics
         "error: actions without enough detailed instructions to be executed, might be described elsewhere, however the person running the protocol is expected to know what this means.")
(tag-doc 'protc:function '(ilxtr:deprecated-tag ilxtr:aspect)
         "Prefer `protc:symbolic-measure` which makes more sense in the context of `protc:*measure`. Usually an analysis function that converts data -> data. [3-15-18: The text that these target are usually apsects, often complex aspects that must be measured on symbolic inputs. The proper way to tag them is probably as protc:aspect ilxtr:hasInformationInput with the text box holding links to the intput?]")
(tag-doc 'protc:how '(ilxtr:implementation ilxtr:compoundTechnique)
         "Specific details and descriptions of ways to perform each step.")
(tag-doc 'protc:i-have-no-idea 'ilxtr:executorSemantics
         "I understand what is going on, but I have no idea how to break it down into its parts, often VERY dense text that contains tons of information, may imply `protc:delegated-instructions` or `protc:executor-verb`.")
(tag-doc 'protc:implied-aspect 'ilxtr:aspect
         "Use this tag only in a reply to an annotation where there is an implied `protc:aspect` that has no annotateable reference in the text.")
(tag-doc 'protc:implied-input 'ilxtr:participant
         "Use this tag only in a reply to an annotation where there is an implied `protc:input` that has no annotateable reference or anchor in the text.")
(tag-doc 'protc:implied-output 'ilxtr:participant
         "Use this tag only in a reply to an annotation where there is an implied `protc:output` that has no annotateable reference or anchor in the text.")
(tag-doc 'protc:input 'ilxtr:participant
         "Equipment used in the lab, tools, chemical solutions, research subjects, anything that is an input to a step that you need to retrieve or refer to for any reason.")
(tag-doc 'protc:invariant 'ilxtr:parameter
         "Numbers dependent on the procedure, mostly ratios/percentages, not directly measureable. Actualizing an invariant is more complex than actualizing a `protc:parameter*` and will generally involve the construction of a number of `protc:parameter*`s")
(tag-doc 'protc:no-how-error 'protc:error
         "Mistake missing explanation where the instructions are too vague or missing steps")
(tag-doc 'protc:no-parameters-error 'protc:error
         "error: steps given without specific parameters/measurements")
(tag-doc 'protc:no-what-error 'protc:error
         "error: no criteria defined, the referent is nowhere to be found in the paper")
(tag-doc 'protc:objective* '(ilxtr:parameter ilxtr:goal)
         "An ill-defined parameter. Goal of the steps outlined [compare 'cryoprotectant' with 'bath changing times were minimized' >> maybe we need to distinguish instructions from goals? how vs nature of what?]")
(tag-doc 'protc:operator
         "Steps involving addition, division, subtraction, multiplication.")
(tag-doc 'protc:order
         "Prepositions about time and organizing steps.")
(tag-doc 'protc:output 'ilxtr:participant
         "What is obtained from the procedure, specifically physical objects")
(tag-doc 'protc:output-spec
         "Description of the structure of the information to be saved (e.g. neurolucida in (x, y, z, d) out) [THIS IS CONFUSING BECAUSE IT IS FOR NUMBERS NOT PHYSICAL OBJECTS?]")
(tag-doc 'protc:parameter* 'ilxtr:parameter
         "Numbers or specific measurements/locations used for lab settings, directly measureable. Often have units or are 'counts' of things")
(tag-doc 'protc:references-for-use 'ilxtr:citationTag
         "Citations of other literature or research papers (author and year) that are explicitly referenced in a methods section as documentation for a protocol.")
(tag-doc 'protc:repeat 'protc:control-flow
         "Repeat commands or computations for a specific set of varying inputs (i.e. there should be an associated annotation that enumerates said inputs), related to `protc:substitute-input`.")
(tag-doc 'protc:result 'ilxtr:informationArtifact
         "A number generated as the result of a `protc:*measure`.")
(tag-doc 'protc:substitute-input 'protc:control-flow
         "What steps/materials should be replaced in order to conduct a slightly different procedure e.g. a control experiment, should refer back to the original *make* or *measure function")
(tag-doc 'protc:symbolic-input 'ilxtr:informationArtifact
         "The input to a `protc:symbolic-measure`. Should be a symbolic entity such as a data file, number, or other symbol. Not a physical input which should be annotated with `protc:input`. These are distinguished from `protc:parameter*` by the fact that they often exist without the intention or knowledge of how to bind them to a physical entite. In some abstract sense they are parameters but there is no intention to act on them, and often one cannot do so easily (e.g. changing the conversion efficiency of a particular GFP protein requires extensive engineering). There are cases in closed loop systems where these can be the symbolic inputs to a `protc:parameter*` which will then be actualized.")
(tag-doc 'protc:symbolic-measure 'ilxtr:symbolicFunction
         "A measurement that takes symbolic input, specifically `protc:symbolic-input` or `protc:symbolic-output` which correspond to their physical versions `protc:input` and `protc:output`")
(tag-doc 'protc:symbolic-output 'ilxtr:informationArtifact
         "The output of a `protc:*measure` or `protc:symbolic-measure` these probably don't need to be tagged explicitly during curation. Should be a data file, number, or symbol. Not a physical object.")
(tag-doc 'protc:telos 'ilxtr:goal
         "Explaining the purpose of a specific step. Differes from `protc:objective*` in that it may not be directly relevant for determining whether the goal of the current step has been achieved.")
(tag-doc 'protc:impl 'protc:control-flow "protc:impl indicates that the text in question, or its translation into protc sould be contained in an impl section."
