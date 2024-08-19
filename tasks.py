from pathlib import Path
from invoke import task

def generate_documentation(c):
    c.run(f"R -e 'devtools::document()'")

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
    c.run("""R -e 'install.packages("../myClimGui_latest.tar.gz", lib=Sys.getenv("R_LIBS_USER"), repos=NULL, build_vignettes=TRUE)'""")

@task
def generate(c):
    """
    Generate source and documentation
    """
    Path("NAMESPACE").unlink(missing_ok=True)
    generate_documentation(c)

@task
def generate_html(c):
    generate_documentation(c)
    c.run("""R -e 'pkgdown::build_site(override = list(destination = "../docs/gui"))'""")

@task
def check(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::check()'""", pty=True)

@task
def test(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::test()'""", pty=True)

@task
def run_dev(c):
    c.run("""R -e 'devtools::load_all(); mcg_run(myClim::mc_data_example_raw, port=8989)'""")
