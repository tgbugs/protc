from pyontutils.utils import setPS1
from protcur.server import make_server_app

setPS1('protcur')

app = make_server_app()
