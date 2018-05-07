SRCFILES = ./keyserver/keyserver-new.aifom \
 ./keyserver/keyserver.aifom \
 ./pkcs-model-Ming-Ye/01-key_separation.aifom \
 ./pkcs-model-Ming-Ye/02-attack_unset.aifom \
 ./pkcs-model-Ming-Ye/03-attack_unset_revised.aifom \
 ./pkcs-model-Ming-Ye/04-system_phase2.aifom \
 ./pkcs-model-Ming-Ye/06-re_import_att_bind_attributes.aifom \
 ./pkcs-model-Ming-Ye/07-system_verified.aifom \
 ./pkcs-model-Ming-Ye/08-lost_key_att.aifom \
 ./pkcs-model-Ming-Ye/09-lost_key_att_countered.aifom \
 ./NSL/NSL-classic.aifom \
 ./NSL/nsl.aifom \
 ./NSL/NSPK-classic.aifom \
 ./NSL/nspk.aifom \
 ./simpleSecurityDevice.aifom

########################

all:	
	$(foreach file,$(SRCFILES), echo $(file); prow $(file);)
