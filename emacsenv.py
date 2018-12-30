#!/usr/bin/env python3

import sys
import os
import shutil
import subprocess as sp
from pathlib import Path

assert len(sys.argv) > 1

_, cmd, arg1, *rest = sys.argv

env_root_name = '.emacsenv'
env_root_path = Path(os.environ['HOME']) / env_root_name

if cmd == 'init':
    env_name = arg1
    emacsd_path = env_root_path / env_name / '.emacs.d'
    os.makedirs(emacsd_path, exist_ok=True)
    url = 'https://raw.githubusercontent.com/Shellay/EmacsConfig/master/init.el'
    out = sp.check_output(['wget', url, '-O', emacsd_path / 'init.el'])
    print(out)
elif cmd == 'serve':
    env_name = arg1
    overriden_env = os.environ.copy()
    env_path = env_root_path / env_name
    overriden_env['HOME'] = env_path
    out = sp.check_output(['emacs', f'--daemon={env_name}'], env=overriden_env)
    print(out)
elif cmd == 'remove':
    env_name = arg1
    env_path = env_root_path / env_name
    shutil.rmtree(env_path)
else:
    raise Exception(f'Unknown command: {cmd}')
