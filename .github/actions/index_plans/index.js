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

const EMPTY_FOLDER = { files: {}, folders: {} }
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
    const key = path[0];
    const files = {...tree.files, [key]: item}
    return {...tree, files}
  }

  const [parent, ...rest] = path
  const folders =  {...tree.folders, [parent]: putAtPath(tree.folders[parent] || EMPTY_FOLDER, rest, item)}
  return {...tree, folders}
}

async function buildPlanMetadata(plan) {
  const response = await fetch(plan.download_url)
  const text = await response.text()
  const yaml = YAML.parse(text)

  return {
    description: yaml.description,
    author_name: yaml.author.name || yaml.author.email || yaml.author.github,
    download_url: plan.download_url,
    path: plan.path.replace(/^plans\//, ""),
    name: plan.name,
    sha: plan.sha
  }
}

async function run() {
  const plans = await listPlans()

  let index = EMPTY_FOLDER

  for (const plan of plans) {
    if (plan.type != "dir") {
      const planMetadata = await buildPlanMetadata(plan)
      index = putAtPath(index, plan.path, planMetadata)
    }
  }

  fs.writeFileSync("./index.json", JSON.stringify(index))
}

run()

