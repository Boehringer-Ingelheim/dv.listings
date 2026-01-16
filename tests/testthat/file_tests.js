var tests = [
  {
     "name":"Write 0 bytes",
     "actions":[ { "contents":"", "offset":0 } ],
     "result":""
  },
  {
     "name":"Append 0 bytes",
     "actions":[ { "contents":"", "offset":-1 } ],
     "result":""
  },
  {
     "name":"Append 1 byte",
     "actions":[ { "contents":"H", "offset":-1 } ],
     "result":"H"
  },
  {
     "name":"Append 2 bytes, then 0 bytes, then write 0 bytes at the end",
     "actions":[ { "contents":"He", "offset":-1 }, { "contents":"", "offset":-1 }, { "contents":"", "offset":2 }],
     "result":"He"
  },
  {
     "name":"Append 2 bytes, then three bytes",
     "actions":[ { "contents":"He", "offset":-1 }, { "contents":"llo", "offset":-1 } ],
     "result":"Hello"
  },
  {
     "name":"Append 5 bytes, then overwrite the last one ",
     "actions":[ { "contents":"Hello", "offset":-1 }, { "contents":" ", "offset":4 } ],
     "result":"Hell "
  },
  {
     "name":"Overwrite first byte and append one byte",
     "actions":[ { "contents":"Hello", "offset":-1 }, { "contents":"Y", "offset":0 }, { "contents":"w", "offset":-1 }],
     "result":"Yellow"
  },
  {
     "name":"Overwrite one byte and write a few more",
     "actions":[ { "contents":"Hi", "offset":-1 }, { "contents":"ello, world!", "offset":1 }],
     "result":"Hello, world!"
  }
];
