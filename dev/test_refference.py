# /// script
# dependencies = ["pytest", "pytest-icdiff"]
# ///

# Run via pipx: pipx run test_refference.py


import pytest
import sys
import refference


def test_1():
    assert refference.sample1() == {}


if __name__ == '__main__':
    retcode = pytest.main(sys.argv[1:])
    sys.exit(retcode)
