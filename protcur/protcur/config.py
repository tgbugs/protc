import sys
from pathlib import Path
import appdirs

__script_folder__ = Path(__file__).resolve().parent

# units
__units_folder__ = Path(__script_folder__, '../../protc-lib/protc/units')

if not __units_folder__.exists():
    _attempts = [
        Path(__script_folder__, '../resources/units'),
        Path(sys.prefix, 'share', 'protcur', 'units'),
        Path('/usr', 'share', 'protcur', 'units'),  # pypy3
        Path('share', 'protcur', 'units'),  # ebuild testing
        Path(appdirs.user_data_dir(), 'protcur', 'units'),  # pip install --user
    ]
    for attempt in _attempts:
        if attempt.exists():
            __units_folder__ = attempt
            break
    else:
        raise FileNotFoundError('no units folder was found at any of\n'
                                f'{[__units_folder__] + _attempts}')

__units_test_folder__ = __units_folder__ / 'test'
__units_test_params__ = __units_test_folder__ / 'params.rkt'

# tags
__tags_folder__ = Path(__script_folder__, '../../.')
if not (__tags_folder__ / 'protc-tags.rkt').exists():
    _attempts = [
        Path(__script_folder__, '../resources'),
        Path(sys.prefix, 'share', 'protcur'),
        Path('/usr', 'share', 'protcur'),  # pypy3
        Path('share', 'protcur'),  # ebuild testing
        Path(appdirs.user_data_dir(), 'protcur'),  # pip install --user
    ]
    for attempt in _attempts:
        if (attempt / 'protc-tags.rkt').exists():
            __tags_folder__ = attempt
            break
    else:
        raise FileNotFoundError('no tags folder was found at any of\n'
                                f'{[__tags_folder__] + _attempts}')

__anno_tags__ = __tags_folder__ / 'anno-tags.rkt'
__protc_tags__ = __tags_folder__ / 'protc-tags.rkt'
