const input = await Deno.readTextFile('./input.txt')

const commandArray: [string, number][] = input.split('\n').map(l => { 
  let [c, u] = l.split(' ')
  return [c, Number(u)] 
})

const position: { 
  horizontal: number, 
  depth1: number,
  depth2: number,
  aim: number,
} = {
  horizontal: 0,
  depth1: 0,
  depth2: 0,
  aim: 0,
}

for (let [command, units] of commandArray) {
  switch (command) {
    case 'forward':
      position.horizontal += units
      position.depth2 += position.aim * units
      break
    case 'down':
      position.depth1 += units
      position.aim += units
      break
    case 'up':
      position.depth1 -= units
      position.aim -= units
      break
  }
}

console.log('Part 1: ', position.horizontal * position.depth1)
console.log('Part 2: ', position.horizontal * position.depth2)