build-action:
  npm run build --prefix .github/actions/index_plans

act-action:
  act -W .github/workflows/index_plans.yml -s GITHUB_TOKEN=$GITHUB_TOKEN

test-action: build-action act-action
