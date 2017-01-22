# Rpackages-lwlegopt
##Some tools may be  used  for the data processing of cancer genomes
This tool is used to plot 3D-lego image for mutation spectrum.
#Instructions
#1
>library(lwlegopt)

>data(Mutdata)

>Draw_lego(h)

#2
>library(lwlegopt)

>Draw_lego(path="example",file_name='TCGA.originalGenomes_with_types.txt')

# 3
>library(lwlegopt)

>Draw_lego(path="example",file_name='combined_mutations.out.xls',title='CRC',prepare=TRUE,samtools='/ifshk1/BC_CANCER/01bin/01bin/samtools/samtools',fasta='/ifshk1/BC_CANCER/01bin/DNA/software/pipeline/CSAP_v5.2.7/Database/human_19/hg19_fasta_GATK/hg19.fasta')

#data preparing
For # 3 , Inputfile which generated by MUTECT has the following form��

>contig	position	context	ref_allele	alt_allele	tumor_name	normal_name	score	dbsnp_site	covered	power	tumor_power	normal_power	normal_power_nsp	normal_power_wsp	total_reads	map_Q0_reads	init_t_lod	t_lod_fstar	t_lod_fstar_forward	t_lod_fstar_reverse	tumor_f	contaminant_fraction	contaminant_lod	t_q20_count	t_ref_count	t_alt_count	t_ref_sum	t_alt_sum	t_ref_max_mapq	t_alt_max_mapq	t_ins_count	t_del_count	normal_best_gt	init_n_lod	normal_f	n_q20_count	n_ref_count	n_alt_count	n_ref_sum	n_alt_sum	power_to_detect_positive_strand_artifact	power_to_detect_negative_strand_artifact	strand_bias_counts	tumor_alt_fpir_median	tumor_alt_fpir_mad	tumor_alt_rpir_median	tumor_alt_rpir_mad	observed_in_normals_count	failure_reasons	judgement

>chr1	17296739	CTTxTGT	C	T	136869-17	HZYblood	0	NOVEL	COVERED	0.992926	0.992926	1	1	1	151	0	107.6196	107.6196	88.747485	32.232969	0.5	0.02	2.326076	63	31	31	1120	1115	60	60	0	0	CC	26.15388	0	85	87	0	3102	0	0.999999	1	(21,13,25,10)	52	21	37	24	0		KEEP

>chr1	19969975	AAAxTTA	G	A	136869-17	HZYblood	0	NOVEL	COVERED	0.981955	0.981955	1	1	1	155	0	92.045794	92.107961	19.248768	83.141925	0.464286	0.02	2.38739	57	30	26	1117	965	60	60	0	0	GG	29.473437	0	96	98	0	3559	0	1	0.998117	(6,25,6,23)	59	21	40	23	0		KEEP

>chr1	151258755	TCTxGGA	G	T	136869-17	HZYblood	0	NOVEL	COVERED	1	1	1	1	1	239	0	26.606803	44.195015	39.662284	9.59092	0.125	0.02	4.634466	129	112	16	4153	575	60	60	0	0	GG	32.802707	0	107	109	0	4016	0	0.985573	0.99995	(85,40,14,4)	44.5	22	54.5	22	0		KEEP