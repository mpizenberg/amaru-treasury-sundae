This is the frontend app to help manage the Amaru Treasury.
This repo contains symbolic links, so will not work on Windows.
This repo contains git submodules so make sure they are up-to-date.

```sh
git clone --recursive git@github.com:mpizenberg/amaru-treasury-sundae.git
cd amaru-treasury-sundae/

# build the aiken code
cd amaru-treasury/
make help     # for insights
make scopes
make permissions
make treasury
make registry

# build the frontend code
cd ../frontend/
npm install
npm run make   # needed at least once
npm run watch  # the hot-reload dev server
```
