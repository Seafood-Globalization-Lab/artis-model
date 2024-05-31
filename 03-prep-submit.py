
import os
import re

hs_versions = ['96', '02', '07', '12', '17']

# read in zorro set up file
f = open('00-zorro-hpc-setup.R')
zorro_setup_content = f.read()
f.close()

# read in original file
f = open('02-artis-pipeline.R', 'r')
artis_file_content = f.read()
f.close()

submit_folder = 'zorro_submit_files'
if not os.path.isdir(submit_folder):
  os.mkdir(submit_folder)

for hs_version in hs_versions:
  
  new_zorro_setup_content = re.sub('hs_version_run <- \"17\"', 'hs_version_run <- \"' + hs_version + '\"', zorro_setup_content)
  new_artis_content = re.sub('# source\(\"/project/ARTIS/Package/00-zorro-hpc-setup.R\"\)', 'source("/project/ARTIS/Package/00-zorro-hpc-setup_HS' + hs_version +'.R")', artis_file_content)
  new_artis_content = re.sub('source\(\"00-demo-setup.R\"\)', '# source\(\"00-demo-setup.R\"\)', new_artis_content)
  
  # create new Zorro setup file
  curr_setup_filename = '00-zorro-hpc-setup_HS' + hs_version + '.R'
  curr_setup_fp = os.path.join(submit_folder, curr_setup_filename)
  curr_setup_f = open(curr_setup_fp, 'w')
  curr_setup_f.write(new_zorro_setup_content)
  curr_setup_f.close()
  
  print('Done writing ' + curr_setup_filename)
  
  # create new ARTIS pipeline file
  curr_filename = 'ARTIS_pipeline_HS' + hs_version + '.R'
  curr_fp = os.path.join(submit_folder, curr_filename)
  curr_f = open(curr_fp, 'w')
  curr_f.write(new_artis_content)
  curr_f.close()
  
  print('Done writing ' + curr_filename)
  
print('Files ready for Zorro submission!')
