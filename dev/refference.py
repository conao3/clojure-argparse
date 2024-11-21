import argparse
import re
from typing import Any


def sample1(*command_line_args: str) -> dict[str, Any]:
    parser = argparse.ArgumentParser()
    args = parser.parse_args(command_line_args)
    return vars(args)


def sample2(*command_line_args: str) -> dict[str, Any]:
    parser = argparse.ArgumentParser()
    parser.add_argument('foo', type=int)
    parser.add_argument('bar', type=int)
    parser.add_argument('-b', type=int)
    args = parser.parse_args(command_line_args)
    return vars(args)


def parse_args() -> argparse.Namespace:
    candidate = [elm for elm in globals().keys() if re.match(r'^sample\d+$', elm)]

    parser = argparse.ArgumentParser()
    parser.add_argument('-m', '--main', choices=candidate)
    parser.add_argument('args', nargs='*')

    return parser.parse_args()


def main():
    args = parse_args()
    if args.main:
        fn = globals().get(args.main)
        assert fn
        print(fn(*args.args))


if __name__ == '__main__':
    main()
