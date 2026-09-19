"""Retabulate frozen derived registries only; no patient inputs or analysis."""
from pathlib import Path
import csv, hashlib, json, sys
project=Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parents[3]
pkg=Path(__file__).resolve().parents[1];out=pkg/'inst/extdata'
handoff=project/'GBM_ACT6_CURRENT_CLAUDE_EVIDENCE_HANDOFF_2026-09-11'
registry=project/'GBM标签数据库/GBM_unified_registry_de_novo_benchmark_AuthorityV2_2026-07-23/02_authority/registry_authority_v2/extracted/GBM_legacy46_reconstructed_authority_v2_2026-07-23'
sources=[]
def read(p):
 b=p.read_bytes();sources.append(dict(path=str(p.relative_to(project)),sha256=hashlib.sha256(b).hexdigest(),byte_size=len(b)))
 with p.open() as f:return list(csv.DictReader(f,delimiter='\t'))
def write(name,rows,fields):
 with (out/(name+'.tsv')).open('w') as f:
  w=csv.DictWriter(f,fieldnames=fields,delimiter='\t',lineterminator='\n',extrasaction='ignore');w.writeheader()
  for row in rows:w.writerow({k:('NA' if row.get(k) in [None,''] else row[k]) for k in fields})
manifest=read(handoff/'resource/PRIMARY201_MODEL_MANIFEST_FROZEN.tsv')
membership=read(handoff/'resource/PRIMARY201_GENE_MEMBERSHIP_FROZEN.tsv')
crosswalk=read(project/'GBM_SIGNATURE_3D_CROSS_LAYER_MEASUREMENT_VALIDATION_V6_FINAL/00_governance/PRIMARY201_GENE_CROSSWALK_FROZEN.tsv')
models=read(registry/'11_UNIFIED_MODEL_INSTANCE_REGISTRY_AUTHORITY_V2.tsv');pubs=read(registry/'10_UNIFIED_PUBLICATION_REGISTRY_AUTHORITY_V2.tsv')
locks=read(handoff/'act5/02_ACT5_V2_SIGNATURE_IDENTITY_LOCK.tsv');den=read(handoff/'act5/06_ACT5_V2_COMPLETE_STAGEB1_DENOMINATOR.tsv')
counts=read(project/'GBM_CURRENT_CURATED_RELEASE_2026-08-22/COUNT_AUTHORITY.tsv')
nonrep=read(project/'GBM_FINAL_AUTHORITY_PROPAGATION_AND_MANUSCRIPT_FREEZE_PREP_2026-09-06/05_methods_and_evaluability/PRIMARY201_REPRESENTABILITY_CASCADE.tsv')
mi={r['model_instance_id']:r for r in models};pi={r['publication_id']:r for r in pubs};ids={r['model_instance_id'] for r in manifest}
cw={r['source_scoring_feature_id']:r for r in crosswalk};genes=[];seen=set()
for r in membership:
 c=cw.get(r['scoring_feature_id']);assert c is not None,r
 symbol=c['canonical_gene_symbol'];key=(r['model_instance_id'],symbol)
 assert symbol not in ['','NA'],r
 if key in seen:continue # Frozen many-source-to-one crosswalk collapses identical canonical membership.
 seen.add(key);genes.append(dict(model_instance_id=key[0],gene_symbol=symbol,mapping_rule_id=c['mapping_method'],resolved='TRUE'))
write('GENE_MEMBERSHIP_LONG',genes,['model_instance_id','gene_symbol','mapping_rule_id','resolved'])
# Individual membership hashes use the exact bytes of deterministic sorted symbol lines;
# rule is recorded explicitly and is resource identity, not an engine representation hash.
ghash={i:hashlib.sha256(('\n'.join(sorted(g['gene_symbol'] for g in genes if g['model_instance_id']==i))+'\n').encode()).hexdigest() for i in ids}
rows=[]
for r in manifest:
 i=r['model_instance_id'];m=mi[i];p=pi[r['publication_id']]
 rows.append(dict(model_instance_id=i,model_id=i,curation_lineage=r['source_stratum'],publication_id=r['publication_id'],doi_or_pmid=p['doi'] or p['pmid'],source_stratum=r['source_stratum'],feature_type=r['feature_type'],n_genes_published=r['original_feature_n'],n_genes_mapped=sum(g['model_instance_id']==i for g in genes),mapping_rule_id='FROZEN_V6_CROSSWALK_CANONICAL_UNIQUE_SORTED',gene_membership_hash=ghash[i],scoring_rule_id='UNSIGNED_UNWEIGHTED_MEAN_PRESENT_GENE_ZSCORES',claim_family=None,eligibility_status=m['current_eligibility'],flags=None,provenance_hash=r['bulk_formal_input_source_sha256']))
write('PRIMARY201_MANIFEST',rows,list(rows[0]))
cascade=[('publications_to_instances',265,271,'ENUMERATE_MODEL_INSTANCES','Multiple model instances per publication; not an exclusion'),('eligible_retained',271,242,'AUTH_CURRENT_ELIGIBILITY','current_eligibility is ELIGIBLE or ELIGIBLE_RETAINED'),('complete_membership',242,232,'AUTH_COMPLETE_MEMBERSHIP','Frozen complete membership count; scope boundaries recorded separately'),('primary_operator',232,201,'FROZEN_OPERATOR_PARTITION','232 = 201 PRIMARY + 17 SENSITIVITY17 + 8 EXCLUDED/OFFSCOPE + 6 NONREPRESENTABLE'),('primary_pre',201,37,'PRE37','Pre-freeze stratum; disjoint union with POST164'),('primary_post',201,164,'POST164','Post-freeze stratum; disjoint union with PRE37'),('sensitivity',232,17,'SENSITIVITY17','Additional frozen sensitivity set; not Primary201')]
write('DERIVATION_CASCADE',[dict(step_id=x[0],from_n=x[1],to_n=x[2],rule_id=x[3],rule_text=x[4]) for x in cascade],['step_id','from_n','to_n','rule_id','rule_text'])
excluded=[];nonmap={r['model_instance_id']:r for r in nonrep}
for m in models:
 i=m['model_instance_id']
 if i in ids:continue
 reason=nonmap[i]['reason_not_representable_by_frozen_scoring_rule'] if i in nonmap else m['final_terminal_status']
 excluded.append(dict(exclusion_id='EX-'+i,model_instance_id=i,rule_id='FROZEN_OPERATOR_NONREPRESENTABLE' if i in nonmap else 'AUTHORITY_REGISTRY_DISPOSITION',reason=reason,stratum=m['source_stratum'],source_file=str((registry/'11_UNIFIED_MODEL_INSTANCE_REGISTRY_AUTHORITY_V2.tsv').relative_to(project)),source_row_key=i))
write('EXCLUSION_LEDGER',excluded,['exclusion_id','model_instance_id','rule_id','reason','stratum','source_file','source_row_key'])
# No automatic claim inference: this is the frozen StageB1 capability question.
claim_id='STAGEB1_DS03_Q100_RECOVERY';design='STAGEB1_DS03_BLOCKED_Q100'
claim=[dict(claim_id=claim_id,claim_family='positive_recovery',claim_class='MEASUREMENT_CAPABILITY',claim_text='Recovery of the declared positive alternative at q100 under the frozen DS03 blocked design.',context_id='DS03_MALIGNANT_ONLY',source_file=str((handoff/'act5/06_ACT5_V2_COMPLETE_STAGEB1_DENOMINATOR.tsv').relative_to(project)))]
write('CLAIM_REGISTRY',claim,list(claim[0]))
evalrows=[]
for r in den:
 ok=r['q100_exact_alpha_reachable']=='YES'
 evalrows.append(dict(model_instance_id=r['signature_id'],claim_id=claim_id,design_id=design,evaluable='TRUE' if ok else 'FALSE',stop_reason='NOT_APPLICABLE' if ok else 'ALPHA_UNREACHABLE',context='DS03_MALIGNANT_ONLY',denominator=8,source_file=str((handoff/'act5/06_ACT5_V2_COMPLETE_STAGEB1_DENOMINATOR.tsv').relative_to(project)),source_row_key=r['source_row_key']))
write('EVALUABILITY_MATRIX',evalrows,list(evalrows[0]))
reps=[]
for r in locks:
 if r['signature_id'] not in ids:continue
 reps.append(dict(representation_id=r['signature_id']+'::DS03_MALIGNANT_ONLY',model_instance_id=r['signature_id'],object_type='UNWEIGHTED_MEMBERSHIP',original_object=r['original_representation'],original_formula=r['original_formula'],aggregation_rule='MEAN',transformation='ZSCORE_WITHIN_CONTEXT',preprocessing=r['audited_representation'],missing_feature_policy='DROP',derived_from_original='TRUE',equivalence_status='NOT_EQUIVALENT' if r['representation_equivalent']=='NO' else 'UNKNOWN',representation_hash=None,representation_rule_hash=None))
write('REPRESENTATION_REGISTRY',reps,list(reps[0]))
write('AMENDMENT_REGISTRY',[dict(amendment_id='ACT6_RESOURCE_RETABULATION_001',**{'class':'A1'},date='2026-09-12',date_relative_to_results='AFTER_RESULTS',reason='Schema retabulation of frozen derived resources; no new biological calculations',affected_tables='PRIMARY201_MANIFEST;GENE_MEMBERSHIP_LONG;DERIVATION_CASCADE;EXCLUSION_LEDGER;EVALUABILITY_MATRIX;REPRESENTATION_REGISTRY;CLAIM_REGISTRY')],['amendment_id','class','date','date_relative_to_results','reason','affected_tables'])
(pkg/'data-raw/SOURCE_MANIFEST.json').write_text(json.dumps(sources,indent=2)+'\n')
assert len(rows)==201 and len(ids)==201 and len(models)==271 and len(pubs)==265
assert sum(m['current_eligibility'] in ['ELIGIBLE','ELIGIBLE_RETAINED'] for m in models)==242
assert sum(m['complete_gene_membership']=='YES' for m in models)==232
assert len(excluded)==70
print('Retabulated 201 manifest rows,',len(genes),'canonical membership rows,',len(reps),'explicit representation records,',len(evalrows),'evaluability records.')

# Vocabulary files are frozen schema inputs; include them in deterministic receipts.
import hashlib
files=sorted(p for p in (pkg/'inst/extdata').rglob('*') if p.is_file() and p.name!='HASHES.tsv')
with (pkg/'inst/extdata/HASHES.tsv').open('w',newline='') as h:
 w=csv.writer(h,delimiter='\t',lineterminator='\n');w.writerow(['file_path','sha256','byte_size'])
 for p in files:w.writerow([p.relative_to(pkg/'inst/extdata').as_posix(),hashlib.sha256(p.read_bytes()).hexdigest(),p.stat().st_size])
