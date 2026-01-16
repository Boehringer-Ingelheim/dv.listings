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
    
    
    OUT(`Storage path: ${list_v.path}`);
    
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
      
      if(contents !== test.result){
        OUT(`Error: Test '${test.name}' failed. Expected '${test.result}' but produced '${contents}' instead`);
        return;
      }
    }
    
    OUT("Success!!!");
  });
  
  /*

  // Read file
  document.getElementById("read-file").addEventListener("click", async () => {
    try {
      const paths = Object.keys(dv_fsa.g_cached_listing).filter(
        (key) => dv_fsa.g_cached_listing[key].kind === "file"
      );
      const status = await dv_fsa.read({ status_input_id: "read-status", paths });
      OUT(JSON.stringify(status, null, 2));
    } catch (error) {
      OUT(`Error: ${error.message}`);
    }
  });

  // Write file
  document.getElementById("write-file").addEventListener("click", async () => {
    try {
      const filePath = "example.txt";
      const contents = btoa("Hello, File System Access API!");
      const status = await dv_fsa.write(filePath, contents, -1); // Append mode
      OUT(JSON.stringify(status, null, 2));
    } catch (error) {
      OUT(`Error: ${error.message}`);
    }
  });
  
  */
});

/*
// Test Suite for File System Access API
(async () => {
  console.log("Running Test Suite...");

  // Test: List folder contents
  try {
    const status = await dv_fsa.list({ status_input_id: "test-folder-status" });
    console.assert(status.list, "Folder listing failed");
    console.log("Folder listing test passed:", status);
  } catch (error) {
    console.error("Folder listing test failed:", error.message);
  }

  // Test: Read file
  try {
    const paths = Object.keys(dv_fsa.g_cached_listing).filter(
      (key) => dv_fsa.g_cached_listing[key].kind === "file"
    );
    const status = await dv_fsa.read({ status_input_id: "test-read-status", paths });
    console.assert(Object.keys(status.contents).length > 0, "File reading failed");
    console.log("File reading test passed:", status);
  } catch (error) {
    console.error("File reading test failed:", error.message);
  }

  // Test: Write file
  try {
    const filePath = "test-example.txt";
    const contents = btoa("Test content for File System Access API!");
    const status = await dv_fsa.write(filePath, contents, -1); // Append mode
    console.assert(!status.error, "File writing failed");
    console.log("File writing test passed:", status);
  } catch (error) {
    console.error("File writing test failed:", error.message);
  }

  console.log("Test Suite Completed.");
})();
*/