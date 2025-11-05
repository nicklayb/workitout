import * as core from '@actions/core'
import { context, getOctokit } from '@actions/github'
import { readFile } from 'node:fs/promises'
import * as YAML from 'yaml'
import fetch from 'node-fetch'
import fs from 'fs'

const OWNER = 'nicklayb'
const REPO = 'workitout'
const PLANS_PATH = "plans"
const BRANCH = "plans"

let octokitSingleton = null

function getOctokitSingleton() {
  if (octokitSingleton) {
    return octokitSingleton;
  }
  const githubToken = core.getInput('token');
  octokitSingleton = getOctokit(githubToken);
  return octokitSingleton;
}

async function listPlans() {
  const response = await getOctokitSingleton().rest.repos.getContent({
    owner: OWNER,
    repo: REPO,
    path: PLANS_PATH,
    ref: BRANCH
  })

  return response.data
}

function putAtPath(tree, path, item) {
  if (path.constructor == String) {
    return putAtPath(tree, path.replace(/^plans\//, "").split("/"), item)
  }

  if (path.length == 1) {
    return { ...tree, [path[0]]: item }
  }
  const [parent, ...rest] = path
  
  return {...tree, [parent]: putAtPath(tree[parent] || {}, rest, item)}
}

async function buildPlanMetadata(plan) {
  const response = await fetch(plan.download_url)
  const text = await response.text()
  const yaml = YAML.parse(text)

  return {
    description: yaml.description,
    author_name: yaml.author.name || yaml.author.email || yaml.author.github,
    download_url: plan.download_url,
    sha: plan.sha
  }
}

async function run() {
  const plans = await listPlans()

  let index= {}

  for (const plan of plans) {
    const planMetadata = await buildPlanMetadata(plan)
    index = putAtPath(index, plan.path, planMetadata)
  }

  console.log(JSON.stringify(index))

  fs.writeFileSync("./index.json", JSON.stringify(index))
}

run()

