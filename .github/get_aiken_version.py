import tomllib

with open("amaru-treasury/aiken.toml", "rb") as f:
    data = tomllib.load(f)
    print(f"{data['compiler']}")
