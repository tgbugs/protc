from pathlib import Path
__script_folder__ = Path(__file__).resolve().parent
__units_folder__ = Path(__script_folder__, '../../protc-lib/protc/units')

if not __units_folder__.exists():
    __units_folder__ = Path('/usr/share')

__units_test_folder__ = __units_folder__ / 'test'
__units_test_params__ = __units_test_folder__ / 'params.rkt'
