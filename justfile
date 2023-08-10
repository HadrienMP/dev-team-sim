# Start in dev mode
start:
  yarn start

# Install dependencies
install:
  yarn install

# Launch automated tests
test *FLAGS:
  yarn test {{FLAGS}}

tcr:
  ./tcr.sh
