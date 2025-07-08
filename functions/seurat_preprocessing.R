library(Matrix)
library(DropletUtils)
library(Seurat)
library(tidyverse)
library(rtracklayer)

path <- "/srv/baia/prj/pvsca/data/so_repro_data/results/STARSOLO"
ref_file <- "/srv/baia/prj/pvsca/data/so_repro_data/ref/PlasmoDB-68_PvivaxP01.gff"

# Import rRNA IDs
rRNA <- rtracklayer::import.gff3(ref_file) %>%
  as.data.frame() %>%
  filter(type == "rRNA") %>%
  select(ID, description) %>%
  mutate(ID = gsub("\\.1", "", ID))

# Functions
remove_rRNA <- function(seurat_obj, rRNA_ids = rRNA$ID) {
  genes_to_keep <- !(rownames(seurat_obj) %in% rRNA_ids)
  subset(seurat_obj, features = rownames(seurat_obj)[genes_to_keep])
}

filter_emptyDrops <- function(seurat_obj, fdr_thresh = 0.001, min_total = 40) {
  ed_out <- emptyDrops(GetAssayData(seurat_obj, layer = "counts"))
  keep_cells <- which(ed_out$FDR < fdr_thresh & ed_out$Total >= min_total)
  subset(seurat_obj, cells = colnames(seurat_obj)[keep_cells])
}


# Load data with ReadMtx
load_seurat <- function(data_path, project_name) {
  counts <- ReadMtx(
    mtx = file.path(data_path, "matrix.mtx"),
    features = file.path(data_path, "features.tsv"),
    cells = file.path(data_path, "barcodes.tsv")
  )
  CreateSeuratObject(counts = counts, project = project_name)
}

# Paths for datasets
dataset_paths <- list(
  Chesson_Spz = file.path(path, "Chesson_spz/Solo.out/Gene/raw"),
  PvBSa = file.path(path, "PvBS_Sa/Solo.out/Gene/raw"),
  PvLSCamIsolate = file.path(path, "PvLsCamIsolate/Solo.out/Gene/raw"),
  PvSPZCamIsolate = file.path(path, "PvSpz_CamIsolate/Solo.out/Gene/raw")
)

# Load Seurat objects
seurat_list <- map2(dataset_paths, names(dataset_paths), load_seurat)

# Remove rRNA genes from all objects
seurat_list <- lapply(seurat_list, remove_rRNA, rRNA_ids = rRNA$ID)

# Remove emptyDrops filtered cells from all objects
seurat_list <- lapply(seurat_list, filter_emptyDrops)

# Add metada
chesson_spz_seurat$dataset <- "Chesson_Spz"
pvbsa_seurat$dataset <- "PvBSa"
pvlscamisolate_seurat$dataset <- "PvLSCamIsolate"
pvspzcamisolate_seurat$dataset <- "PvSPZCamIsolate"





# Normalize
chesson_spz_seurat <- NormalizeData(chesson_spz_seurat)
pvbsa_seurat <- NormalizeData(pvbsa_seurat)
pvlscamisolate_seurat <- NormalizeData(pvlscamisolate_seurat)
pvspzcamisolate_seurat <- NormalizeData(pvspzcamisolate_seurat)

# Scale data
chesson_spz_seurat <- ScaleData(chesson_spz_seurat, features = VariableFeatures(chesson_spz_seurat))
pvbsa_seurat <- ScaleData(pvbsa_seurat, features = VariableFeatures(pvbsa_seurat))
pvlscamisolate_seurat <- ScaleData(pvlscamisolate_seurat,
  features = VariableFeatures(pvlscamisolate_seurat)
)
pvspzcamisolate_seurat <- ScaleData(pvspzcamisolate_seurat,
  features = VariableFeatures(pvspzcamisolate_seurat)
)


# Find variable feature
chesson_spz_seurat <- FindVariableFeatures(chesson_spz_seurat,
  selection.method = "vst", nfeatures = 2000
)
pvbsa_seurat <- FindVariableFeatures(pvbsa_seurat, selection.method = "vst", nfeatures = 2000)
pvlscamisolate_seurat <- FindVariableFeatures(pvlscamisolate_seurat,
  selection.method = "vst", nfeatures = 2000
)
pvspzcamisolate_seurat <- FindVariableFeatures(pvspzcamisolate_seurat,
  selection.method = "vst", nfeatures = 2000
)


# Not running yet
# Run PCA
chesson_spz_seurat <- RunPCA(chesson_spz_seurat, features = VariableFeatures(chesson_spz_seurat))
pvbsa_seurat <- RunPCA(pvbsa_seurat, features = VariableFeatures(pvbsa_seurat))
pvlscamisolate_seurat <- RunPCA(pvlscamisolate_seurat, features = VariableFeatures(pvlscamisolate_seurat))
pvspzcamisolate_seurat <- RunPCA(pvspzcamisolate_seurat, features = VariableFeatures(pvspzcamisolate_seurat))

# Run UMAP
chesson_spz_seurat <- RunUMAP(chesson_spz_seurat, dims = 1:20)
pvbsa_seurat <- RunUMAP(pvbsa_seurat, dims = 1:20)
pvlscamisolate_seurat <- RunUMAP(pvlscamisolate_seurat, dims = 1:20)
pvspzcamisolate_seurat <- RunUMAP(pvspzcamisolate_seurat, dims = 1:20)

# Run tSNE
chesson_spz_seurat <- RunTSNE(chesson_spz_seurat, dims = 1:20)
pvbsa_seurat <- RunTSNE(pvbsa_seurat, dims = 1:20)
pvlscamisolate_seurat <- RunTSNE(pvlscamisolate_seurat, dims = 1:20)
pvspzcamisolate_seurat <- RunTSNE(pvspzcamisolate_seurat, dims = 1:20)


# Inintegrated workflow
seurat_list <- list(
  Chesson_Spz = chesson_spz_seurat,
  PvBSa = pvbsa_seurat,
  PvLSCamIsolate = pvlscamisolate_seurat
)

# Normalize and find variable feature
seurat_list <- lapply(seurat_list, SCTransform, verbose = TRUE)

# Find inegration anchors
anchors <- FindIntegrationAnchors(object.list = seurat_list, dims = 1:20)

# Integrate data
integrated_seurat <- IntegrateData(anchorset = anchors, dims = 1:20)

# Downstream analysis
integrated_seurat <- ScaleData(integrated_seurat, verbose = TRUE)
integrated_seurat <- RunPCA(scaledata, npcs = 30, verbose = TRUE)
integrated_seurat <- RunUMAP(integrated_seurat, dims = 1:20)
integrated_seurat <- RunTSNE(integrated_seurat, dims = 1:20)
