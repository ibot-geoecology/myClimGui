from pathlib import Path
from invoke import task

@task
def build(c):
    """
    build myClimGui package
    """
    c.run("""R -e 'devtools::build(".", path="../myClimGui_latest.tar.gz")'""")


@task
def install(c, vignette=True):
    """
    install myClimGui
    """
    build(c)
    c.run("""R -e 'install.packages("../myClimGui_latest.tar.gz", repos=NULL)'""")

@task
def generate(c):
    """
    Generate source and documentation
    """
    Path("NAMESPACE").unlink(missing_ok=True)
    c.run(f"R -e 'devtools::document()'")

@task
def check(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::check()'""", pty=True)

@task
def test(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::test()'""", pty=True)

@task
def run_dev(c):
    c.run("""R -e 'devtools::load_all(); mcg_run(myClim::mc_data_example_agg, port=8989)'""")
