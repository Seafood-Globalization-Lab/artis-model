
import os
import re

hs_versions = ['96', '02', '07', '12', '17']

# read in original file
f = open('02-artis-pipeline.R', 'r')
artis_file_content = f.read()
f.close()

submit_folder = 'zorro_submit_files'
if not os.path.isdir(submit_folder):
  os.mkdir(submit_folder)

for hs_version in hs_versions:
  
  new_artis_content = re.sub('hs_version_run <- \"17\"', 'hs_version_run <- \"' + hs_version + '\"', artis_file_content)
  
  # create new file name
  curr_filename = 'ARTIS_pipeline_HS' + hs_version + '.R'
  curr_fp = os.path.join(submit_folder, curr_filename)
  curr_f = open(curr_fp, 'w')
  curr_f.write(new_artis_content)
  
  print('Done writing ' + curr_filename)
  
print('Files ready for Zorro submission!')
