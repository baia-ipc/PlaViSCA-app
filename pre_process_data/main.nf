
process STAR_INDEX {
    input:
        path fasta
        path gtf

    output:
        path "STAR_INDEX", emit: index_dir

    when:
        !file("STAR_INDEX/SAindex").exists()

    script:
    """
        mkdir -p STAR_INDEX
        STAR \
            --runThreadN ${task.cpus} \
            --runMode genomeGenerate \
            --genomeDir STAR_INDEX \
            --genomeFastaFiles $fasta \
            --sjdbGTFfile $gtf \
            --sjdbOverhang 99 \
            --genomeSAindexNbases 11
    """
}


process STARSOLO {

    publishDir "results/STARSOLO", mode: 'copy'

    input:
        path index_dir
        path gtf
        tuple val(sample_id), path(read1), path(read2)
        path whitelist

    output:
        path "${sample_id}_*"

    script:
    """
        STAR \
            --runThreadN ${task.cpus} \
            --genomeDir $index_dir \
            --readFilesIn ${read1} ${read2} \
            --soloCBwhitelist ${whitelist} \
            --sjdbGTFfile $gtf \
            --readFilesCommand zcat \
            --soloType CB_UMI_Simple \
            --soloBarcodeMate 1 \
            --clip5pNbases ${params.clip5p} \
            --soloCBlen 16 \
            --soloUMIlen ${params.umi_len} \
            --soloUMIfiltering MultiGeneUMI \
            --alignIntronMin 1 \
            --alignIntronMax 2756 \
            --soloFeatures Gene GeneFull \
            --outFileNamePrefix "${sample_id}_"
    """
}


workflow {
    fasta_ch     = Channel.value(file(params.fasta))
    gtf_ch       = Channel.value(file(params.gtf))
    whitelist_ch = Channel.value(file(params.whitelist))

    fastq_ch = Channel
        .fromFilePairs(params.fq, flat: false, checkIfExists: true)
        .map { srr_id, reads ->
            prj_name = reads[0].getParent().getName()
            sample_id = "${prj_name}_${srr_id}"
            tuple(sample_id, reads[0], reads[1])
        }

    star_index_ch = STAR_INDEX(fasta_ch, gtf_ch)
    STARSOLO(star_index_ch, gtf_ch, fastq_ch, whitelist_ch)
}
