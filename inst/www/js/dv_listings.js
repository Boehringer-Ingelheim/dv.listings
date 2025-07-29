const dv_listings = (function () {

  const latest_reviews_json_idx = 4;
  const latest_review_idx = 1;
  const role_idx = 2;
  const row_number_idx = 0;

  const render_HTML_review = function (json_string) {
    let data;
    try {
      if (typeof json_string === 'string') {
        data = JSON.parse(json_string);
      } else {
        data = json_string;
      }
    } catch (e) {
      return '<em>Invalid JSON</em>';
    }

    const reviews = data.reviews;
    if (!reviews) return '<em>No reviews</em>';

    const data_timestamp = data.data_timestamp || 0;
    let formatted_data_timestamp;
    if (data_timestamp) {
      formatted_data_timestamp = new Date(data_timestamp * 1000).toLocaleString();
    } else {
      formatted_data_timestamp = 'No data timestamp';
    }

    function is_empty_object(obj) {
      return obj && typeof obj === 'object' && Object.keys(obj).length === 0;
    }

    // Find latest timestamp
    let latest_timestamp = -Infinity;
    Object.values(reviews).forEach(r => {
      if (!is_empty_object(r.timestamp) && !isNaN(r.timestamp) && r.timestamp > latest_timestamp) {
        latest_timestamp = r.timestamp;
      }
    });

    let html_inner = '';
    let conflict = 'false';
    let previous_review = '';

    Object.entries(reviews).forEach(([key, r]) => {
      const role = key;

      let latest_label = '';
      if (r.timestamp === latest_timestamp) {
        latest_label = '<span class="label label-primary" style="font-size: 0.75em;">Latest</span>';
      }

      let outdated_label = '';
      if (!is_empty_object(r.timestamp) && r.timestamp < data_timestamp) {
        outdated_label = '<span class="label label-default" style="font-size: 0.75em;">Outdated</span>';
      }

      let timestamp;
      if (!is_empty_object(r.timestamp) && !isNaN(r.timestamp)) {
        timestamp = new Date(r.timestamp * 1000).toLocaleString();
      } else {
        timestamp = '';
      }

      let review_text;
      let label_class;
      if (r.review) {
        review_text = r.review;
        label_class = "label-info";
      } else {
        review_text = 'Not reviewed';
        label_class = "label-default";
      }

      html_inner += `
        <div><strong>${role}</strong></div>
        <div>
          <span class="label ${label_class}" style="display: block; font-size: 1em; padding: 2px 5px;">
            ${review_text}<br>
            <span class="text-muted" style="font-size: 0.75em;">${timestamp}</span>
          </span>
        </div>
        <div>${latest_label} ${outdated_label}</div>
      `;
    });

    let html = `          
      <div class="review-grid" style="display: inline-grid; grid-template-columns: auto auto auto; gap: 6px; align-items: center; font-size: 0.9em;">
      ${html_inner}
      <div style="grid-column: span 3; font-size: 0.8em; color: #666; padding-top: 6px;">
        <strong>Data Timestamp:</strong> ${formatted_data_timestamp}
      </div>
    </div>    
    `;

    return html;
  }

  const render_identity = function (data, type, row, meta) {
    return (data);
  }

  const render_selection = function (id, role, choices) {
    let in_f = function (data, type, row, meta) {
      if (type === 'display') {
        let result = '<div style="display: flex; align-items: baseline; gap: 0.5rem;">';
        result += `<input type="checkbox" data-for-row="${row[row_number_idx]}" data-input-type="bulk-control" onchange = "dv_listings.on_change_table_checkbox(event)">`;
        let options = choices;
        result += `<select onchange=\"Shiny.setInputValue('${id}', {row:${row[row_number_idx]}, option:this.value}, {priority: 'event'});\">`;
        for (let i = 0; i < options.length; i += 1) {
          result += `<option value=${i + 1}${options[i] == data ? ' selected' : ''}>${options[i]}</option>`;
        }
        result += '</select></div>';

        return result;
      } else {
        return data;
      }
    }
    return (in_f);
  }

  const render_status = function (id, role, options) {
    let in_f = function (data, type, row, meta) {
      if (type === 'display') {
        const review_data = JSON.parse(row[latest_reviews_json_idx]);
        const current_role_review = review_data.reviews[role].review;
        const outdated = review_data.reviews[role].timestamp < review_data.data_timestamp;
        const add_confirm_button = (current_role_review !== row[latest_review_idx] && row[role_idx] !== role) ||
          (outdated && row[role_idx] !== role);

        let label_class = '';

        if(data === "OK") {
          label_class = "label-success"
        } else if (data === "Conflict" || data === "Conflict I can fix" || data === "Latest Outdated") {
          label_class = "label-warning";
        } else {
          label_class = "label-default";        
        }

        let result = `
        <div style="width: 100px">
        <div class = "label ${label_class}"> ${data} </div>
        <div>
        <button class = "btn btn-primary btn-xs" style=\"width:100%%\" onclick=\"dv_listings.show_child(event, this, '${meta.settings}')\" title="Show detailed review info">\u{1F4CB}</button>
        `;

        if (add_confirm_button) {
          result += `
          <button class = "btn btn-primary btn-xs" style=\"width:100%%\" onclick=\"Shiny.setInputValue('${id}', {row:${row[row_number_idx]}, option:'${options.indexOf(row[latest_review_idx]) + 1}'}, {priority: 'event'})\" title="Agree with latest review">\u2714</button>          
          `
        }
        result += `</div></div>`

        return result;
      } else {
        return data;
      }
    }
    return (in_f);
  }

  const review_column_render = function (data, type, row, meta) {
    if (type === "display") {
      return render_HTML_review(data);
    } else {
      return (data);
    }
  }

  const show_child = function (event, btn) {
    const tr = $(event.target.closest('tr'));
    const dt_table = $(tr).closest('table').DataTable();
    const row = dt_table.row(tr);

    if (row.child.isShown()) {
      row.child.hide();
      tr.removeClass('shown');
    } else {
      const json = row.data()[4];
      row.child(`
        <div class="child-wrapper" style="display: none;">
          <div class="panel panel-default" style="margin: 0; margin-left: 20px; display: inline-block">
            <div class="panel-body">
              ${render_HTML_review(json)}
            </div>
          </div>
        </div>
      `).show();
      const wrapper = row.child().find('.child-wrapper');
      wrapper.slideDown(200);
      tr.addClass('shown');         
    }
  }

  const render_bulk_menu = function(id, choices, input_id) {    
    const container = $("#" + id).find('.top');

    let choices_menu = '';
    let options = choices;
    choices_menu += `<select>`;
        for (let i = 0; i < options.length; i += 1) {
          choices_menu += `<option value=${i + 1}${i == 0 ? ' selected' : ''}>${options[i]}</option>`;
        }
    choices_menu += '</select>';

    const select_all_visible = `
    <input type="checkbox" onchange="dv_listings.on_change_select_all_checkbox('${id}')">
    `;

    const apply_bulk_split = `
    <div class="btn-group" style = "display:inline-flex">
    <button type="button" class="btn" 
            onclick="dv_listings.apply_bulk_visible('${id}', '${input_id}')">
      Apply selected
    </button>
    <button type="button" class="btn btn-primary dropdown-toggle" data-toggle="dropdown" aria-expanded="false">
      <span class="caret"></span>
      <span class="sr-only">Toggle Dropdown</span>
    </button>
    <ul class="dropdown-menu" role="menu">
      <li>
        <a href="#" onclick="dv_listings.apply_bulk_full('${id}', '${input_id}'); return false;">
          Apply full table
        </a>
      </li>
    </ul>
  </div>    
    `;

    const html = `
      <div class='bulk-menu-wrapper'>
        ${select_all_visible}
        ${choices_menu}
        ${apply_bulk_split}
        
      </div>`;
      // ${apply_bulk_full}
      //   ${apply_bulk_visible}

    container.prepend(html);
  }

  const get_all_select_checkbox = function(container_id) {
    const inputs = $("#" + container_id).find('input[data-input-type="bulk-control"]');
      return(inputs);
  }

  const compute_select_all_checkbox_state = function (container_id) {
    const inputs = get_all_select_checkbox(container_id);
    let checkbox_state;
    let all_false;
    let all_true;    
    if (inputs.length > 0) {
      all_false = true;
      all_true = true;    
      for (let i = 0; i < inputs.length; i++) {
        all_false = all_false && !inputs[i].checked;
        all_true = all_true && inputs[i].checked;
      }      
    } else {
      all_false = true;
      all_true = false;    
    }
    checkbox_state = {checked: all_true, indeterminate: !all_false&&!all_true}
    return (checkbox_state);    
  };

  const set_select_all_checkbox_state = function (state, container_id) {    
    const checkbox = document.querySelector(`#${container_id} .bulk-menu-wrapper input[type="checkbox"]`);
    if(checkbox!==null) {
      checkbox.checked = state.checked;
      checkbox.indeterminate = state.indeterminate;    
    }    
  };

  const refresh_bulk_select_all_checkbox = function(container_id) {
    const state = compute_select_all_checkbox_state(container_id);
    set_select_all_checkbox_state(state, container_id);
  };

  const on_change_table_checkbox = function (event) {
    const container_id = event.target.closest(".dataTables_wrapper").id;
    const state = compute_select_all_checkbox_state(container_id);
    set_select_all_checkbox_state(state, container_id);
  };

  const on_change_select_all_checkbox = function (container_id) {    
    const inputs = get_all_select_checkbox(container_id);    
    const current_state = compute_select_all_checkbox_state(container_id);
    let next_state;

    if(current_state.indeterminate || !current_state.checked) {
      for (let i = 0; i < inputs.length; i++) {
        inputs[i].checked = true;
      }
      next_state = {checked: true, indeterminate: false};
    } else if(current_state.checked) {
      for (let i = 0; i < inputs.length; i++) {
        inputs[i].checked = false;
      }
      next_state = {checked: false, indeterminate: false};
    }

    set_select_all_checkbox_state(next_state, container_id);
  };

  const apply_bulk_visible = function(container_id, input_id) {
    const inputs = $("#" + container_id + " input[data-input-type='bulk-control']:checked");
    const choice_value = $("#" + container_id + " .top select").val();
    let selected_row_ids = [];
    for (let i = 0; i < inputs.length; i++) {
      const row_id = inputs[i].getAttribute('data-for-row');  
      selected_row_ids.push(row_id);  
    }    
    Shiny.setInputValue(input_id, {row:selected_row_ids, option:choice_value}, {priority: 'event'})
  };

  const apply_bulk_full = function(container_id, input_id) {        
    const choice_value = $("#" + container_id + " .top select").val();    
    Shiny.setInputValue(input_id, {row:null, option:choice_value, bulk:'filtered'}, {priority: 'event'})
  };

  const res = {
    review_column_render: review_column_render,
    render_selection: render_selection,
    render_identity: render_identity,
    render_status: render_status,
    render_bulk_menu: render_bulk_menu,
    refresh_bulk_select_all_checkbox: refresh_bulk_select_all_checkbox,
    on_change_select_all_checkbox: on_change_select_all_checkbox,
    on_change_table_checkbox: on_change_table_checkbox,
    apply_bulk_visible: apply_bulk_visible,
    apply_bulk_full: apply_bulk_full,
    show_child: show_child
  }
  return (res)
})()

const dv_fsa = (function() {

  const g_directory = {handle: null, error: "unattached"};  
  let overlay_id = "dv_fsa_overlay";

  const attach = async function({status_input_id}) {
    show_overlay({message: "Attaching..."});
    if(window.showDirectoryPicker) {
      g_directory.handle = null;
      g_directory.error = null;

      try {
        g_directory.handle = await window.showDirectoryPicker({mode: 'readwrite'});
      } catch (error) {
        g_directory.error = 'Could not gain write access to folder' ;
      }    
      
    } else {
      g_directory.error = 'The File System Access API is not supported by this browser';
    }  

    let status = { 
      connected: g_directory.handle !== null, 
      name: g_directory.handle !== null ? g_directory.handle.name: null, 
      error: g_directory.error
    };

    Shiny.setInputValue(status_input_id, status, {priority: 'event'});    
    hide_overlay();
  };

  const _assert_init_and_attached = function() {    
    if(g_directory.error === "unattached") throw new Error("g_directory is `unattached`. call `init`");
    return;
  };

  const _Array_fromAsync = async function(asyncIterator){ 
    const arr=[]; 
    for await(const i of asyncIterator) arr.push(i); 
    return arr;
  };

  const _get_file = async function (dir_handle, file_name, mode){
    _assert_init_and_attached();
    let res = {handle: null, error: null};
  
    let flags = null
    if(mode == 'read') flags = {};
    else if(mode == 'write') flags = {create: true}; // TODO: Remove
    else if(mode == 'append') flags = {create: true};
    else debugger;
  
    if(dir_handle.error){
      res.error = 'Could not open ' + file_name + ' because folder failed with error: ' + dir_handle.error;
    } else {
      try {
        res.handle = await g_directory.handle.getFileHandle(file_name, flags); // FileSystemFileHandle {kind: 'file', name: 'hello_world.txt'}
      } catch(error) {
        res.error = 'Error getting handle of `' + file_name + '`: ' + error.message
      }
    }
    return res;
  };

  const _read_file = async function(file_handle){
    let res = null;
    if(!file_handle.error){

      try {
        let file = await file_handle.handle.getFile()
        res = await file.arrayBuffer();
      } catch(error) {
        debugger; // TODO: message
        file_handle.error = 'Could not read contents of handle'
      }
    }
    return res;
  };

  const _write_file = async function(file_handle, contents){ // TODO: Remove
    if(!file_handle.error){      
      try{
        const writable = await file_handle.handle.createWritable();
        await writable.write(contents);
        await writable.close();
      } catch(error){
        file_handle.error = 'Could not write file: ', error.message;
      }
    }
  };

  const _append_file = async function(file_handle, contents){
    if(!file_handle.error){
      try{
        let writable = await file_handle.handle.createWritable({keepExistingData:true});
        let offset = (await file_handle.handle.getFile()).size
        writable.seek(offset)
        await writable.write(contents);
        await writable.close();
      } catch(error){
        file_handle.error = 'Could not append to file: ', error.message;
      }
    }
  };

  // From https://stackoverflow.com/a/66046176
  const _buffer_to_base64 = async function (buffer) {
    // use a FileReader to generate a base64 data URI
    const base64url = await new Promise(r => {
      const reader = new FileReader()
      reader.onload = () => r(reader.result)
      reader.readAsDataURL(new Blob([buffer]))
    });
    // remove `data:...;base64,` prefix
    return base64url.slice(base64url.indexOf(',') + 1);
  };
  
  // TODO: Use Uint8Array.fromBase64() instead, once it becomes available in chrome-derived browsers:
  //       https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/fromBase64
  const _base64_to_buffer = async function (base64) {
    return await (await fetch("data:application/octet;base64," + base64)).arrayBuffer();
  };

  const _read_all_contents = async function (subfolder_candidates) {    
    const results = {};
  
    for (const subfolder of subfolder_candidates) {
      let folder_handle;
      try {
        folder_handle = await g_directory.handle.getDirectoryHandle(subfolder, { create: false });
      } catch (err) {
        console.warn(`Subfolder "${subfolder}" does not exist, skipping.`);
        continue;
      }
  
      results[subfolder] = {};
  
      for await (const [file_name, file_handle] of folder_handle.entries()) {
        if (file_handle.kind === 'file') {
          try {
            const file = await file_handle.getFile();
            const contents = await _buffer_to_base64(await file.arrayBuffer());
            results[subfolder][file_name] = {
              size: file.size,
              time: file.lastModified / 1000,
              contents: contents,
              error: null
            };
          } catch (err) {
            console.warn(`Failed to read "${subfolder}/${file_name}": ${err.message}`);
            results[subfolder][file_name] = {
              size: null,
              time: null,
              contents: null,
              error: `Failed to read "${subfolder}/${file_name}": ${err.message}`
            };
          }
        }
      }  
      
      if (Object.keys(results[subfolder]).length === 0) {
        delete results[subfolder];
      }
    }
  
    return results;
  }

  const _execute_IO_plan = async function(plan) {
    for (let idx=0; idx<plan.length;idx++) {
      let entry = plan[idx];
      if(entry.type === "create_dir") {
        try {
          await g_directory.handle.getDirectoryHandle(entry.dname, {create: true});          
          entry.error = null;
        } catch (error) {
          entry.error = "Error creating dir: " + entry.path;
          console.error(entry.error);
        }
      } else if (entry.type === "write_file") {
        // FIXME(miguel)? This abstraction sits unnaturally close to the FSA API.
        //                The `entry` could be simply {full_path + contents}; no need for dir+fname or mode
        if(entry.mode === "bin"){
          try {
            const buffer = await _base64_to_buffer(entry.contents);
            const dir_handle = await g_directory.handle.getDirectoryHandle(entry.path, {create: false});
            const file_handle = await dir_handle.getFileHandle(entry.fname, {create: true});
  
            const writable = await file_handle.createWritable();
            await writable.write(buffer);
            await writable.close();
            entry.error = null;
          } catch (error) {
            debugger;
            entry.error = "Error writing: " + entry.path + "/" + entry.fname;
            console.error(entry.error);
          } finally {
            entry.contents = null;
          }
        } else {
          console.error("Uknown mode: " + entry.mode)
        }        
      } else if(entry.type === "append_file") {
        // FIXME(miguel)? This abstraction sits unnaturally close to the FSA API.
        //                The `entry` could be simply {full_path + contents}; no need for dir+fname or mode
        if(entry.mode === "bin"){
          try {
            const buffer = await _base64_to_buffer(entry.contents);
            const dir_handle = await g_directory.handle.getDirectoryHandle(entry.path, {create: false});
            const file_handle = await dir_handle.getFileHandle(entry.fname, {create: false});
            const file = await file_handle.getFile();
            const file_contents = new Uint8Array(await file.arrayBuffer());

            const combined_contents = new Uint8Array(file_contents.length + buffer.byteLength);
            combined_contents.set(file_contents, 0);
            combined_contents.set(new Uint8Array(buffer), file_contents.length);

            const temp_file_name = entry.fname + "_" + crypto.randomUUID() + ".tmp"  
            const temp_handle = await dir_handle.getFileHandle(temp_file_name, {create: true});
            const writable = await temp_handle.createWritable();
            await writable.write(combined_contents);
            await writable.close();
            await temp_handle.move(dir_handle, entry.fname);

            entry.error = null;
          } catch (error) {
            entry.error = "Error writing: " + entry.path + "/" + entry.fname;
            console.error(entry.error);
          } finally {
            entry.contents = null;
          }
        } else {
          console.error("Uknown mode: " + entry.mode)
        }        
      } else {        
        console.error("Uknown type: " + entry.type)
      }
    }

    return(plan);
  }

  const list = async function ({ status_input_id, folder }) {
    _assert_init_and_attached();
    let entries_async = g_directory.handle.entries();
    let entries = await _Array_fromAsync(entries_async);

    let list = {};
    for (let i = 0; i < entries.length; i++) {
      let [name, handle] = entries[i];

      if (handle.kind === 'file') {
        try {
          let file = await handle.getFile();
          list[name] = {
            kind: 'file',
            size: file.size,
            time: file.lastModified / 1000
          };
        } catch (err) {
          console.warn(`Failed to read file "${name}": ${err.message}`);
        }
      } else if (handle.kind === 'directory') {
        list[name] = {
          kind: 'directory'          
        };
      }
    }

    let status = {
      list: list,
      error: g_directory.error
    };
    Shiny.setInputValue(status_input_id, status, { priority: 'event' });
    return (status);
  };

  const read = async function({status_input_id, file_name}){
    _assert_init_and_attached();
    show_overlay({message: "Reading file..."});
    let file = await _get_file(g_directory, file_name, 'read');
    let contents_binary = await _read_file(file);
    let contents = null;
    
    if(!file.error){
      let t0 = Date.now()
      contents = await _buffer_to_base64(contents_binary)
      let t1 = Date.now()
      console.log('Time in base64 encoding: '+(t1-t0)/1000)
    }
    
    let status = {file_name: file_name, contents: contents, error: file.error};
    
    let t0 = Date.now()
    Shiny.setInputValue(status_input_id, status, {priority: 'event'});
    let t1 = Date.now()
    console.log('Time in setInputValue: '+(t1-t0)/1000)
    hide_overlay();
    return(status);
  };

  const read_folder = async function({status_input_id, subfolder_candidates}){
    _assert_init_and_attached();
    show_overlay({message: "Reading all files..."});
    let status = await _read_all_contents(subfolder_candidates);
        
    let t0 = Date.now()
    Shiny.setInputValue(status_input_id, status, {priority: 'event'});
    let t1 = Date.now()
    console.log('Time in setInputValue: '+(t1-t0)/1000)
    hide_overlay();
    return(status);
  };
  
  const append = async function ({status_input_id, file_name, contents}){
    // TODO: Rename file as temp; append and move back?
    show_overlay({message: "Appending to file..."});
    let file = await get_file(g_directory, args.file_name, 'append');
    // TODO: check size_known_to_client
    
    let contents_base64 = await _base64_to_buffer(contents);
    await _append_file(file, contents_base64);
    
    if(file.error){
      console.error(file.error);
      alert('Error appending to file: ' + file.error);
    }
    
    let status = {name: file_name, error: file.error};
    Shiny.setInputValue(status_input_id, status, {priority: 'event'});
    hide_overlay();
  };

  const write = async function ({status_input_id, file_name, contents}){
    show_overlay({message: "Writing file..."});
    // TODO: Rename file as temp; append and move back?
    let file = await get_file(g_directory, args.file_name, 'append');
    // TODO: check size_known_to_client
    
    let contents_base64 = await _base64_to_buffer(contents);
    await _write_file(file, contents_base64);
    
    if(file.error){
      console.error(file.error);
      alert('Error appending to file: ' + file.error);
    }
    
    let status = {name: file_name, error: file.error};
    Shiny.setInputValue(status_input_id, status, {priority: 'event'});
    hide_overlay("Writing file...");
  };

  const execute_IO_plan = async function({status_input_id, plan, is_init}) {
    _assert_init_and_attached();
    const status = await _execute_IO_plan(plan);
    const res = {
      status: status,
      is_init: is_init
    }    
    Shiny.setInputValue(status_input_id, res, {priority: 'event'});
  }

  const init = function() {    
    Shiny.addCustomMessageHandler('dv_fsa_attach', attach);
    Shiny.addCustomMessageHandler('dv_fsa_list', list);
    Shiny.addCustomMessageHandler('dv_fsa_read', read);
    Shiny.addCustomMessageHandler('dv_fsa_write', write);
    Shiny.addCustomMessageHandler('dv_fsa_append', append);    
    Shiny.addCustomMessageHandler('dv_fsa_show_overlay', append);
    Shiny.addCustomMessageHandler('dv_fsa_remove_overlay', append);
    Shiny.addCustomMessageHandler('dv_fsa_read_folder', read_folder);
    Shiny.addCustomMessageHandler('dv_fsa_execute_io_plan', execute_IO_plan);
  };

  const show_overlay = function ({message}) {
    hide_overlay();
    const overlay = document.createElement('div');
    overlay.style.position = 'fixed';
    overlay.style.top = '0';
    overlay.style.left = '0';
    overlay.style.width = '100vw';
    overlay.style.height = '100vh';
    overlay.style.backgroundColor = 'rgba(0, 0, 0, 0.5)';
    overlay.style.display = 'flex';
    overlay.style.justifyContent = 'center';
    overlay.style.alignItems = 'center';
    overlay.style.color = 'white';
    overlay.style.fontSize = '2rem';
    overlay.style.zIndex = '10000';
    overlay.textContent = message;
    overlay.id = overlay_id;
  
    document.body.appendChild(overlay);
  };
  
  const hide_overlay = function(_) {
    const overlay = document.getElementById(overlay_id);
    if (overlay) {
      document.body.removeChild(overlay);
    }
  };
    
  const res = {
    init: init,
    attach: attach,    
    list: list,
    read: read,
    append: append,

    write: write
  };

  return (res);

})();

dv_fsa.init();

// Listeners cannot call the picker for security reasons,
// Therefore we must attach manually after a user click
// Possible if a database is available
