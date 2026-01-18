// module JS code assumes `Shiny` is available, so we mock it
const Shiny = {
  setInputValue: (id, value, options) => {
    console.log(`Mock Shiny.setInputValue called with id: ${id}, value:`, value, ", options:", options);
  },
  addCustomMessageHandler: (id, handler) => {
    console.log(`Mock Shiny.addCustomMessageHandler called with id: ${id}`);
  }
};

document.addEventListener("DOMContentLoaded", () => {
  const outputText = document.getElementById("output-text");

  // Helper function to display output
  const OUT = (message) => {
    outputText.textContent += message + "\n";
  };
  
  const _base64_to_buffer = async function (base64) {
    return await (await fetch("data:application/octet;base64," + base64)).arrayBuffer();
  };

  // List folder contents
  document.getElementById("do-test").addEventListener("click", async () => {
    let list_v = null;
    try {
      list_v = await dv_fsa.list({ status_input_id: "folder-status" });
    } catch (error) {
      OUT(`Error: ${error.message}`);
      return;
    }
    
    if(list_v.error !== null){
      OUT(`Error: ${list_v.error}`);
      return;
    }
    
    if(Object.keys(list_v.list).length > 0){
      OUT(`Error: This test requires an empty folder`);
      return;
    }
    
    
    OUT(`Storage path: ${list_v.path}\n`);
    
    for(let i_test = 0; i_test < tests.length; i_test += 1){
      let test = tests[i_test];
      let fname = './test_'+i_test+'.txt';
      for(let i_action = 0; i_action < test.actions.length; i_action += 1){
        let action = test.actions[i_action];
        await dv_fsa.write(fname, btoa(action.contents), action.offset)
      }
      
      let status = await dv_fsa.read({status_input_id:'dummy_status_id', paths:[fname]});
      let contents_b64 = status.contents[fname];
      let contents = contents_b64;
      if(contents.length > 0){
        contents = await (await fetch("data:application/octet;base64," + contents)).text();
      }
      
      await dv_fsa.remove(fname);
      
      if(contents == test.result){
        OUT(`${test.name}: OK.`);
      } else {
        OUT(`${test.name}: Error. Expected '${test.result}' but produced '${contents}' instead`);
        return;
      }
    }
    
    OUT("\nSuccess!!!");
  });
});