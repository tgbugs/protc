import sys
from pathlib import Path
import orthauth as oa

auth = oa.configure_here('auth-config.py', __name__)

# units
__units_folder__ = auth.get_path('units-folder')
__units_test_folder__ = __units_folder__ / 'test'
__units_test_params__ = __units_test_folder__ / 'params.rkt'

# tags
__tags_folder__ = auth.get_path('tags-folder-file').parent
__anno_tags__ = __tags_folder__ / 'anno-tags.rkt'
__protc_tags__ = __tags_folder__ / 'protc-tags.rkt'
