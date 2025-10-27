import fs from 'fs'

const PLAN_FOLDER = "./plans/"

const reduceDirectory = (directory, acc, func) => {
  const items = fs.readdirSync(directory, { recursive: true })
  for (const item of items) {
    const fullPath = path.join(directory, item)
    acc = func(fullPath, acc)
  }

  return acc
}

const getPlans = (directory) => {
  const acc = reduceDirectory(directory, {}, (file) => {
    const content = fs.readFileSync(file)

  })
}

const json = getPlans(PLAN_FOLDER)

console.log(json)
