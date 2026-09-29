import * as core from '@actions/core'
import { readFile } from 'node:fs/promises'
import * as YAML from 'yaml'
import fetch from 'node-fetch'
import fs from 'fs'
import fsPromise from 'node:fs/promises';
import nodePath from 'node:path';

const OWNER = 'nicklayb'
const REPO = 'workitout'
const PLANS_PATH = "plans"
const BRANCH = "plans"

let octokitSingleton = null

const EMPTY_FOLDER = { files: {}, folders: {} }

const ROOT = "./plans"

const buildPath = filepath => nodePath.join(ROOT, filepath)

const isDirectory = path => {
  return fs.lstatSync(buildPath(path)).isDirectory() 
}

async function listPlans() {
  const content = await fsPromise.readdir(ROOT, { recursive: true })
  console.log(content)

  return content
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
  const filepath = buildPath(plan)
  const text = await fsPromise.readFile(filepath, { encoding: "utf8" })
  const yaml = YAML.parse(text)

  return {
    description: yaml.description,
    author_name: yaml.author.name || yaml.author.email || yaml.author.github,
    download_url: "",
    path: plan.replace(/^plans\//, ""),
    name: nodePath.basename(plan),
    sha: ""
  }
}

async function run() {
  const plans = await listPlans()

  let index = EMPTY_FOLDER

  for (const plan of plans) {
    if (!isDirectory(plan)) {
      const planMetadata = await buildPlanMetadata(plan)
      index = putAtPath(index, plan, planMetadata)
    }
  }

  fs.writeFileSync("./index.json", JSON.stringify(index))
}

run()

