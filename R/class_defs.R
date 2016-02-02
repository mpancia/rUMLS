setClassUnion("charOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClass("Atom", representation(
 aui = "character",
 suppressible = "logical",
 rootSource = "character",
 termType = "character",
 codeURL = "character",
 conceptURL = "character",
 sourceConceptURL = "charOrNULL",
 sourceDescriptor = "charOrNULL",
 attributesURL = "charOrNULL",
 parentsURL = "charOrNULL",
 childrenURL = "charOrNULL",
 relations = "charOrNULL",
 name = "character",
 language = "character",
 definitionsURL = "charOrNULL"
)
)

setClass("Concept", representation(
  cui = "character",
  suppressible = "logical",
  dateAdded = "Date",
  majorRevisionDate = "Date",
  status = "character",
  atomCount = "numeric",
  attributeCount = "numeric",
  cvMemberCount = "numeric",
  atomsURL = "character",
  definitionsURL = "charOrNULL",
  relationsURL = "character",
  semanticTypes = "list",
  preferredAtom = "Atom",
  relationCount = "numeric",
  name = "character",
  atoms = "listOrNULL",
  relations = "listOrNULL"
))

setClass("Relation", representation(
  rui = "character",
  headui = "charOrNULL",
  tailui = "charOrNULL",
  suppressible = "logical",
  sourceui = "charOrNULL",
  obsolete = "logical",
  sourceOriginated = "logical",
  rootSource = "character",
  relationLabel = "character",
  additionalRelationLabel = "charOrNULL",
  groupId = "charOrNULL",
  attributeCount = "numeric"
))

setClass("ConceptRelation", representation(
  relatedConceptURL = "character"
), contains = "Relation")

setClass("AtomRelation", representation(
  relatedAtomURL = "character"
), contains = "Relation")

