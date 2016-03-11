# Update format ID of some objects I inserted as ISO but meant to insert as XML

pids_to_fix <- structure(list(file = c("./acadis-gateway/SeaIceThickness_PrecisionAccuracyAliasing/Precision,_Accuracy,_and_Aliasing/Ice_Survey/GIS_Remote_Sensing/files/03072013_Large_MODIS.tif.aux.xml",
                                      "./acadis-gateway/SeaIceThickness_PrecisionAccuracyAliasing/Precision,_Accuracy,_and_Aliasing/Ice_Survey/GIS_Remote_Sensing/files/rsat2_20130310.0409_large.tif.aux.xml",
                                      "./acadis-field-projects/ARCSS/106.ARCSS031/data/individual_files_bb1_depths.shp.xml",
                                      "./acadis-field-projects/ARCSS/106.ARCSS031/data/individual_files_bb1_coast.shp.xml",
                                      "./acadis-field-projects/ARCSS/106.ARCSS031/data/individual_files_bath_noaa.tif.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.020/data/Fe_Data_H10701.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.079/doc/HE0701_Chlordata.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.202/data/export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.138/doc/metadata.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.229/data/export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.056/data/xml_export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.057/data/xml_export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.119/data/xml_export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.104/data/export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.137/doc/metadata.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.220/data/export.xml",
                                      "./acadis-field-projects/BeringSea/BEST/102.077/data/export.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-003/data/docs_B62_Hollowed_euphausiid.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B53-004/data/docs_SEACAT_BSIERP_ICHTHYO.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-002/data/docs_B52_EcoFOCI_m2_Mooring.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-007/data/docs_B52_EcoFOCI_m5_08BS5B.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-007/data/docs_B52_EcoFOCI_m5_09BS5A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-013/data/B62_Cokelet_Bottom_Trawl_Survey_CTD_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-010/data/docs_B52_EcoFOCI_m8_08BS8A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-015/data/docs_B62-Euphausiids_2009_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-005/data/docs_B67_Kitaysky.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B63-002/data/docs_B63_BLKI_forasgelocations2008-10.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-004/data/docs_B52_EcoFOCI_m4_Mooring.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-011/data/docs_B52_EcoFOCI_m8_08BSV8A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-011/data/docs_B52_EcoFOCI_m8_09BSV8A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-014/data/B62_Cokelet_Bottom_Trawl_Survey_CTD_2010.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B55-001/data/xml_B55_Stoecker_uZoop_Abundance.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-006/data/docs_B67_Trites.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-009/data/docs_B52_EcoFOCI_m8_08BSP8A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B68-001/data/docs_B68_Mueter.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-001/data/docs_B69-Akutan_LTK_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B77-001/data/docs_B77_Jones_Diet_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B77-001/data/docs_B77_Jones_Diet_2008.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B64-003/data/docs_B64_Kuletz_Seabird_Distribution.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-003/data/docs_B52_EcoFOCI_m4_ADCPmoor.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-016/data/docs_B62-Midwater_pollock_2009_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-005/data/docs_B52_EcoFOCI_m2_Mooring.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-008/data/docs_B52_EcoFOCI_m5_08BSV5A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B70-001/data/B70_FEAST_Hindcast_Code.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B65-003/data/docs_B65-Seabird_Colony-based_Studies_2010_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-006/data/docs_B52_EcoFOCI_m5_08BSP5B.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-006/data/docs_B52_EcoFOCI_m5_09BSP5A.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B64-001/data/docs_old_docs_B64_Kuletz.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B55-002/data/xml_B55_Stoecker_uZoop_Grazing.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B66-001/data/docs_B66_Friday.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-003/data/docs_B67_Jay_WalrusBehavior.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B59-003/data/docs_B59_2008-2010_BASIS_BSIERP_v3.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B52-001/data/docs_B52_EcoFOCI_m2_ADCPmoor.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-005/data/docs_B62-Euphausiids_2010_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B56-002/data/docs_B56_Moran_TrapFlux.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B61-002/data/docs_B61_Aydin_2008-2010.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-001/data/docs_B67-At_Sea_PDS_Acoustic_Data_Metafile_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-001/data/docs_B67_Benoit-Bird_Acoustic.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B72-001/data/B72_spatial_economic_models_pollock_cod.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B99-001/data/docs_BSIERP_regions_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B56-001/data/docs_B56_Moran_Radionuclides.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B55-003/data/xml_B55_Stoecker_Abundance_2008-2010.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B74-001/data/2012ForagingTheoryCode_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B74-001/data/2013Middleton_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B74-001/data/2012KittiwakeColonyCode_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B74-001/data/2010CORTprudence_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B77-002/data/docs_B77_Jones_StableIsotope_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B77-002/data/docs_B77_Jones_StableIsotope_2008.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B64-002/data/docs_old_docs_B64-Data_2010.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B73-001/data/xml_B73_Hindcast_output.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-007/data/docs_B69-Togiak_LTK_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B92-001/data/docs_Project_B92_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-005/data/docs_B69-St.Paul_Harvest_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-006/data/docs_B69-St.Paul_LTK_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B66-002/data/docs_B66_2010_metadata_20101228.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B63-003/data/docs_B63_TBMU_divelocations2008-10.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B57-001/data/docs_B57_Grebmeier.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-003/data/docs_B69-Emmonak_Togiak_Akutan_Harvest_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-002/data/docs_B69-Emmonak_LTK_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-007/data/docs_B62_Cokelet_AL09_winter_summer.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-012/data/B62_Cokelet_Bottom_Trawl_Survey_CTD_2008.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B53-003/data/docs_BSIERP_ICHTHYO.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-002/data/docs_B67_Benoit-Bird_CTD.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-002/data/docs_B67-At_Sea_PDS_CTD_Data_Metafile_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B63-001/data/docs_B63_Irons.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-010/data/docs_B62_Cokelet_Oscar_Dyson_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B69-004/data/docs_B69-Savoonga_Harvest_Metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-004/data/docs_B62_Hollowed_pollock.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B67-004/data/docs_B67_Jay_WalrusLipid.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B65-001/data/docs_B65_Byrd_2008.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B65-002/data/docs_B65_Byrd_2009.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-006/data/docs_B62-Midwater_pollock_2010_metadata.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-008/data/docs_B62_Cokelet_AL10_winter_summer.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B70-002/data/B70_FEAST_Hindcast_Fixed_Species.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B70-002/data/B70_FEAST_Hindcast_Output.xml",
                                      "./acadis-field-projects/BeringSea/BSIERP/245.B62-011/data/docs_B62_Cokelet_Oscar_Dyson_2010.xml"
), pid = c("urn:uuid:09b57e96-c8d2-452e-b45b-6d129b0a9822", "urn:uuid:2c390479-18f3-4c94-8bdd-bd4e88192d30",
           "urn:uuid:c7fbbcf9-fafd-42e0-8c18-c732d788371b", "urn:uuid:552014fd-10c5-46e8-ae91-401d1574408c",
           "urn:uuid:7c06708a-d028-4dd6-bd7a-0f0485e66cf1", "urn:uuid:aa07cc00-1f3c-4abd-826c-b9b8d6dc2941",
           "urn:uuid:986bc8e6-9972-47c2-ae95-e3bba162413d", "urn:uuid:5a69fca0-5d1f-4290-907f-f42775c96fd8",
           "urn:uuid:e2dc3b84-85c1-4f4f-8366-202be61e3d76", "urn:uuid:9a37eae2-4a3b-482a-8f51-de57c9261333",
           "urn:uuid:b3793031-5823-440d-a667-bddf1755fe60", "urn:uuid:ceac2fde-d942-409e-8323-3d02a48ed39f",
           "urn:uuid:aeea15c2-3908-4214-8e00-70ab172e8b22", "urn:uuid:cbf66f8a-fb51-4945-a9b9-c8dd4afdf74f",
           "urn:uuid:9b0d63dc-39b7-4a31-83cd-ec70620c134d", "urn:uuid:a7c1c18c-0820-4413-8b35-164b0a817211",
           "urn:uuid:ce3aa891-7cf6-4b4c-909e-18cfd657c5a6", "urn:uuid:de2e24c5-c16f-4e09-91fa-03335740f889",
           "urn:uuid:54354e05-40f0-4969-8c23-2645a9dc5d40", "urn:uuid:fdfcc2f1-dbf4-4cb0-88e7-5f25c6fe481e",
           "urn:uuid:8bf88a96-2c9c-4eb1-bf6b-3e2c6f714da6", "urn:uuid:68400081-554b-4164-a49b-56149236af5e",
           "urn:uuid:6482abb3-b743-43cb-9ff6-89967a1cb238", "urn:uuid:6d1cb397-b038-49fd-8acd-cc9798882ffd",
           "urn:uuid:700ca516-2a06-4324-b70c-fb420cb97a5c", "urn:uuid:ec07997a-266a-4224-915f-b25249a22c9b",
           "urn:uuid:593973fe-7544-41ea-bf0f-9655b35b2945", "urn:uuid:0de6f4fd-c1c3-4f46-b9b8-9b1e379b7e4a",
           "urn:uuid:1698862e-1f42-4af8-90f5-329a837a8cd3", "urn:uuid:17514c1f-adf1-45a7-98e7-0e1569d9377c",
           "urn:uuid:35caac19-6273-473e-9d61-6309af6ac304", "urn:uuid:1ade1f98-cdd7-413d-9ee7-d5a006941c92",
           "urn:uuid:52a4126d-315a-4946-9deb-d6508287d1d1", "urn:uuid:c5c8c406-dc71-43da-9f46-45e3bdd41fbe",
           "urn:uuid:8366c575-2eb1-41b6-8665-cd2daac43402", "urn:uuid:13ca399f-3a84-4c13-984a-bb27fafb0227",
           "urn:uuid:de9716ce-6edb-4705-b7a8-3a146bbb0adb", "urn:uuid:e8acca67-ab5c-404f-91e2-5bcbcd2243e6",
           "urn:uuid:30e73598-4b12-4ec8-8ef4-5660186382a2", "urn:uuid:b81c7f6c-cdb3-4390-9d69-6767a87b71b6",
           "urn:uuid:1885f872-371a-4089-b840-1ab2be56ed04", "urn:uuid:621d08b7-4528-46fd-b47d-b2ecad49b634",
           "urn:uuid:6215f5a0-363b-46c0-9a80-970e56ee1f15", "urn:uuid:7e50d2b4-f683-4aee-b53f-f5d7b24b6716",
           "urn:uuid:3c1f1f8f-653a-414f-9b2d-6ee24bd97d12", "urn:uuid:6476f6eb-5e59-4182-9dba-5828c36ff62b",
           "urn:uuid:e1e18b7d-46a1-47df-b1f9-cbb556c6731f", "urn:uuid:a3aed45c-a81d-4c30-934e-895e06ef2ba3",
           "urn:uuid:95bd54ea-be55-4fb6-ac23-5bdbfec4feaf", "urn:uuid:9da27cd4-8360-4895-bd83-2a59d5af4835",
           "urn:uuid:26033a3d-f2ce-4965-bc9d-304cbb1adfb8", "urn:uuid:f44b2049-324e-4131-9f4f-dc7ae4bf4470",
           "urn:uuid:cdd398d0-4a73-4132-8199-c656083fcf0f", "urn:uuid:dbeb3f32-f0da-49a9-b857-cccde71a8084",
           "urn:uuid:d0244867-6284-436a-80b4-0ccb052f221d", "urn:uuid:12288dd9-0268-4acb-b306-b9f5dce28975",
           "urn:uuid:94e31d90-0190-4564-925c-4ed53e253ce8", "urn:uuid:8e1bba15-848c-43f8-a9fd-64b675e5f448",
           "urn:uuid:2bd2d30a-6d12-4754-ba34-a8c21a939e17", "urn:uuid:6d29dd5c-bdbd-4b15-a848-a6d45e32c25d",
           "urn:uuid:5e4567d1-252e-464f-8b5e-f24cbf464b0e", "urn:uuid:382b3c56-3083-4e0d-8565-6dfa6633ba09",
           "urn:uuid:24d6a2a3-7d16-4b52-a5db-8d128c06e9fd", "urn:uuid:78bfa0a8-308d-456b-8050-5f99ad814290",
           "urn:uuid:f05ac0a2-0478-4eb5-82ec-a30df57b7fd2", "urn:uuid:9965d3f4-ac2b-4cef-85a5-95f9c158621e",
           "urn:uuid:d21d24be-3351-4be3-a142-e4af1587bbfc", "urn:uuid:7bc7b526-191a-4f34-8464-fee5d5f40973",
           "urn:uuid:7169103b-47bf-4c52-a1da-a3deb086af47", "urn:uuid:86071d35-557f-4bf4-89e1-b0ca78b06105",
           "urn:uuid:217d3b7f-0ad9-420b-abc1-17a130958e2b", "urn:uuid:5077376d-f445-4f80-991b-c4d5838dddb3",
           "urn:uuid:1a9d3a6e-bbb2-44d0-9aae-2d84b0dfcbfc", "urn:uuid:ac3b1dcf-bd20-44e2-80c3-2cb2021d9aa2",
           "urn:uuid:4fb479f2-2ee6-40ba-a51f-00701fc6c38e", "urn:uuid:31576944-2a63-47de-ad63-e6ce5764b68b",
           "urn:uuid:66083e26-95c9-466f-aab3-b67e590da9c0", "urn:uuid:1f4af24f-bda6-46cc-b55c-ad37f8402113",
           "urn:uuid:38e66609-9e50-48b1-a6bc-8a01b6db3f5c", "urn:uuid:4820508a-de77-46b7-9586-93aad998983b",
           "urn:uuid:75173c5b-7a9c-41f1-a22e-60625c26e204", "urn:uuid:669ef8fb-77d8-4144-a248-bdef080dd3ae",
           "urn:uuid:1e882936-5646-4e1f-a2ac-4c6dab50a3f9", "urn:uuid:e9900c4e-882a-4580-b761-65952ffd8144",
           "urn:uuid:0d7cd4b1-b2c0-49fe-b50c-d26f56ba8396", "urn:uuid:217f50a0-2545-4bc8-a6a5-b6f72c6035e8",
           "urn:uuid:52b163a8-3939-48a7-afd3-22a2a967cc2a", "urn:uuid:7c6a6017-56d3-42fb-b43c-cf0a7891cb6d",
           "urn:uuid:835e1fb6-e4f8-4ac4-bd8e-1ea23e25fc1c", "urn:uuid:8494a7d8-d3e9-4fe3-9c5b-96f57432a7dd",
           "urn:uuid:3707a044-a65a-4288-8d49-12256ffc759d", "urn:uuid:b4d77d5b-e15c-4cc9-8a48-fc700ec8b08a",
           "urn:uuid:530907c2-6a78-4752-a0f2-ddf3bd0517c2", "urn:uuid:b4650964-7afd-4bf3-a1e0-5956a7a6d4f5",
           "urn:uuid:4ed22412-98a3-4998-ada1-4fd4b639d3b5", "urn:uuid:6cb06bad-9dbc-4eda-a6a4-db008af7a9f0"
)), .Names = c("file", "pid"), class = "data.frame", row.names = c(23028L,
                                                                   23038L, 221296L, 221303L, 221313L, 320173L, 350375L, 350379L,
                                                                   352018L, 394975L, 410355L, 410361L, 427665L, 435787L, 435903L,
                                                                   437109L, 456716L, 456731L, 456736L, 456740L, 456747L, 456748L,
                                                                   456758L, 456764L, 456769L, 456777L, 456794L, 456798L, 456805L,
                                                                   456807L, 456815L, 456820L, 456832L, 456836L, 456842L, 456848L,
                                                                   456854L, 456858L, 456867L, 456874L, 456881L, 456888L, 456895L,
                                                                   456907L, 456912L, 456926L, 456927L, 456936L, 456943L, 456949L,
                                                                   456955L, 456959L, 456965L, 456971L, 456976L, 456981L, 456990L,
                                                                   456991L, 456996L, 457000L, 457012L, 457018L, 457022L, 457024L,
                                                                   457026L, 457029L, 457042L, 457043L, 457051L, 457060L, 457065L,
                                                                   457071L, 457076L, 457082L, 457090L, 457094L, 457104L, 457111L,
                                                                   457119L, 457125L, 457140L, 457147L, 457157L, 457158L, 457164L,
                                                                   457171L, 457175L, 457181L, 457187L, 457194L, 457204L, 457211L,
                                                                   457221L, 457241L, 457247L, 457281L))


library(dataone)
library(datapackage)

mn <- MNode("https://arcticdata.io/metacat/d1/mn/v2")

for (i in 1:nrow(pids_to_fix)) {
  pid <- pids_to_fix[i,"pid"]
  stopifnot(nchar(pid) > 0)

  cat(paste0("Fixing PID ", pid, "\n"))

  url <- paste0("https://arcticdata.io/metacat/d1/mn/v2/meta/", pid)

  response <- try({RCurl::getURL(url)})

  if (inherits(response, "try-error")) {
    cat(paste0("Getting the URL ", url, " failed for some reason. Moving on."))
    next
  }

  doc <- try({XML::xmlParseDoc(response)})

  if (inherits(doc, "try-error")) {
    cat(paste0("Parsing response ", response, " failed for some reason. Moving on."))
    next
  }

  sm <- try({datapackage::parseSystemMetadata(new("SystemMetadata"), xml = XML::xmlRoot(doc))})

  if (inherits(sm, "try-error")) {
    cat(paste0("Parsing sysmeta for ", doc, " failed for some reason. Moving on."))
    next
  }

  sm@formatId <- "application/xml"

  sm_result <- try({updateSystemMetadata(mn, pid, sm)})

  if (inherits(sm, "try-error")) {
    cat(paste0("Updating sysmeta for ", pid, " failed for some reason. Moving on."))
    next
  }

  print(sm_result)

  cat(paste0("Updating system metadata appears to have succeeded for pid ", pid, "\n"))

}


