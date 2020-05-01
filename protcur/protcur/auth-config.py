{'config-search-paths': ['{:user-config-path}/protcur/config.yaml',],
 'auth-variables':
 {'units-folder': {'environment-variables': 'PROTCUR_UNITS_FOLDER',
                   'default': ['../../protc-lib/protc/units',  # git
                               '../resources/units',  # test sdist
                               '{:user-data-path}/protcur/units',  # pip install --user
                               '{:prefix}/share/protcur/units',  # system
                               '/usr/share/protcur/units',  # pypy3
                               '{:cwd}/share/protcur/units',  # ebuild testing
                   ],
 },
  'tags-folder-file': {'environment-variables': 'PROTCUR_TAGS_FOLDER_FILE',  # NOTE you must provide path to the test file
                       'default': ['../../protc-tags.rkt',  # git
                                   '../resources/protc-tags.rkt',  # test sdist
                                   '{:user-data-path}/protcur/protc-tags.rkt',  # pip install --user
                                   '{:prefix}/share/protcur/protc-tags.rkt',  # system
                                   '/usr/share/protcur/protc-tags.rkt',  # pypy3
                                   '{:cwd}/share/protcur/protc-tags.rkt',  # ebuild testing
                       ],
  },
  'hypothesis-api-key': {'environment-variables': 'PROTCUR_HYP_API_KEY HYP_API_KEY HYP_API_TOKEN'},
  'hypothesis-group': {'environment-variables': 'PROTCUR_HYP_GROUP HYP_GROUP'},
  'hypothesis-user': {'environment-variables': 'PROTCUR_HYP_USER HYP_USER'},
 },
}
