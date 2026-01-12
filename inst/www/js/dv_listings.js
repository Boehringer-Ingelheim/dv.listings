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
        latest_label = '<span class="dv-listings-label dv-listings-label-primary" style="font-size: 0.75em;">Latest</span>';
      }

      let outdated_label = '';
      if (!is_empty_object(r.timestamp) && r.timestamp < data_timestamp) {
        outdated_label = '<span class="dv-listings-label dv-listings-label-default" style="font-size: 0.75em;">Outdated</span>';
      }

      let timestamp;
      if (!is_empty_object(r.timestamp) && !isNaN(r.timestamp)) {
        timestamp = new Date(r.timestamp * 1000).toLocaleString();
      } else {
        timestamp = '';
      }

      // FIXME: [BS5] Label classes can be removed once we move to bs5 definitely
      let review_text;
      let label_class;
      if (r.review) {
        review_text = r.review;
        label_class = "dv-listings-label-info";
      } else {
        review_text = 'Not reviewed';
        label_class = "dv-listings-label-default";
      }

      html_inner += `
        <div><strong>${role}</strong></div>
        <div>
          <span class="dv-listings-label ${label_class}" style="display: block; font-size: 1em; padding: 2px 5px;">
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

  /* #why_prevent_default
  There are a few `event.preventDefault()` calls in the inline handlers of interactive elements embedded in the table.

  The problem they solve is that after reviewing data with those controls, the table scrolls back to the beginning.
  That is a big ergonomic failure.

  Now, _crucially_, this **doesn't happen when using the bulk review controls**, even if only for a single row. It 
  doesn't happen either when the code in the `onclick` handler of either the single-row or bulk review controls is 
  executed in the browser's javascript console.

  And, to makes matters more confusing, if the user:
  - clicks on one of the embedded selector dropdowns
  - clicks away _without changing the selection_
  - and then triggers the `onclick` handler from the javascript console
  the table _DOES_ scroll back to the top.

  So, there is _something_ in the table that remembers that there was a click. After that, when there is a 
  `DT::replaceData` that in turn calls an `ajax.reload()` in the client, the scroll position resets to the top.

  The approach we take here is to **silence the default actions of the embedded input elements** that we place on the
  table. The default action for the select dropdown is to... well... drop down, so we restore that one manually through
  a call to showPicker. That API works with firefox and chrome. Support for safari on OSX is coming.

  An alternative way of patching over this issue would be saving and restoring the `scrollTop` position after the ajax
  data reload.
  */

  const render_selection = function (id, role, choices) {
    let in_f = function (data, type, row, meta) {
      if (type === 'display') {
        let result = '<div style="display: flex; align-items: baseline; gap: 0.5rem;">';
        result += `<input type="checkbox" data-for-row="${row[row_number_idx]}" data-input-type="bulk-control" onchange = "dv_listings.on_change_table_checkbox(event)">`;
        let options = choices;
        result += `<select onmousedown=\"event.preventDefault(); event.srcElement.showPicker();\" onchange=\"Shiny.setInputValue('${id}', {row:${row[row_number_idx]}, option:this.value, bulk:'false'}, {priority: 'event'});\">`; // see comment tagged as #why_prevent_default for explanation
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
        const add_confirm_button = (current_role_review !== row[latest_review_idx] && row[role_idx] !== role) || outdated;
          
        let label_class = '';

        if(data === "OK") {
          label_class = "dv-listings-label-success"
        } else if (data === "Conflict" || data === "Conflict I can fix" || data === "Latest Outdated") {
          label_class = "dv-listings-label-warning";
        } else {
          label_class = "dv-listings-label-default";        
        }

        let result = `
        <div style="width: 100px">
        <div class = "dv-listings-label ${label_class}"> ${data} </div>
        <div>
        <button class = "btn btn-primary dv-listings-btn-xs" style=\"width:100%%\" onmousedown=\"event.preventDefault();\" onclick=\"dv_listings.show_child(event, this, '${meta.settings}')\" title="Show detailed review info">\u{1F4CB}</button>
        `; // see comment tagged as #why_prevent_default for explanation

        if (add_confirm_button) {
          result += `
          <button class = "btn btn-primary dv-listings-btn-xs" style=\"width:100%%\" onmousedown=\"event.preventDefault();\" onclick=\"Shiny.setInputValue('${id}', {row:${row[row_number_idx]}, option:'${options.indexOf(row[latest_review_idx]) + 1}', bulk:'false'}, {priority: 'event'})\" title="Agree with latest review">\u2714</button>          
          ` // see comment tagged as #why_prevent_default for explanation
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
      // FIXME:[BS5] Panel classes will be removed once we move definitely to bs5
      row.child(`
        <div class="child-wrapper" style="display: none;">
          <div class="panel panel-default card card-default" style="margin: 0; margin-left: 20px; display: inline-block">
            <div class="panel-body card-body">
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

  const render_bulk_menu_and_undo_button = function(id, choices, bulk_input_id, undo_input_id, undo_description_anchor_id, initial_undo_description) {
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
    <button type="button" class="btn btn-outline-primary" 
            onclick="dv_listings.apply_bulk_visible('${id}', '${bulk_input_id}')">
      Apply selected
    </button>
    <button type="button" class="btn btn-primary dropdown-toggle dropdown-toggle-split" data-bs-toggle="dropdown" data-toggle="dropdown" aria-expanded="false">
      <span class="caret"></span>
      <span class="sr-only">Toggle Dropdown</span>
    </button>
    <ul class="dropdown-menu" role="menu">
      <li>
        <a href="#" onclick="dv_listings.apply_bulk_filtered('${id}', '${bulk_input_id}'); return false;">
          Apply full table
        </a>
      </li>
    </ul>
    </div>    
    `;

    const undo_element = `
    <div class="btn-group" style = "display:inline-flex">
    <button type="button" class="btn btn-outline-primary" onclick="dv_listings.apply_undo('${undo_input_id}')">Undo</button>
    <button type="button" class="btn btn-primary dropdown-toggle dropdown-toggle-split" data-bs-toggle="dropdown" data-toggle="dropdown" aria-expanded="false">
      <span class="caret"></span>
      <span class="sr-only">Toggle Dropdown</span>
    </button>
    <ul class="dropdown-menu" role="menu">
        <div id='${undo_description_anchor_id}'></div>
        ${initial_undo_description}
    </ul>
    </div>    
    `;

    const html = `
      <div class='bulk-menu-wrapper'>
        ${select_all_visible}
        ${choices_menu}
        ${apply_bulk_split}
      </div>
      <div class='undo-button-wrapper'>
        ${undo_element}
      </div>
      `;
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
    Shiny.setInputValue(input_id, {row:selected_row_ids, option:choice_value, bulk:'false'}, {priority: 'event'})
  };

  const apply_bulk_filtered = function(container_id, input_id) {        
    const choice_value = $("#" + container_id + " .top select").val();    
    Shiny.setInputValue(input_id, {row:null, option:choice_value, bulk:'filtered'}, {priority: 'event'})
  };

  const apply_undo = function(input_id) {
    Shiny.setInputValue(input_id, {contents:'dummy'}, {priority: 'event'})
  };

  const res = {
    review_column_render: review_column_render,
    render_selection: render_selection,
    render_identity: render_identity,
    render_status: render_status,
    render_bulk_menu_and_undo_button: render_bulk_menu_and_undo_button,
    refresh_bulk_select_all_checkbox: refresh_bulk_select_all_checkbox,
    on_change_select_all_checkbox: on_change_select_all_checkbox,
    on_change_table_checkbox: on_change_table_checkbox,
    apply_bulk_visible: apply_bulk_visible,
    apply_bulk_filtered: apply_bulk_filtered,
    apply_undo: apply_undo,
    show_child: show_child
  }
  return (res)
})()

const dv_fsa = (function() {
  const FS_WRITE_OFFSET_APPEND = -1;
  let overlay_id = "dv_fsa_overlay";

  let g_directory_handle = null;
  let g_cached_listing = {}; // name (string) : {kind ("directory"/"file"), size (numeric), time (numeric)}
  let g_cached_contents = {}; // name (string) : contents (ArrayBuffer)
  let g_error = "Not listed yet";

  const list = async function({ status_input_id, folder }) {
    g_directory_handle = null;
    g_error = null;
    
    show_overlay({message: "Attaching..."});
    if(window.showDirectoryPicker){
      try {
        g_directory_handle = await window.showDirectoryPicker({mode: 'readwrite'});
        if(g_directory_handle === null) throw new Error("null dir handle");
      } catch (error) {
        g_error = 'Could not gain write access to folder';
      }    
    } else {
      g_error = 'The File System Access API is not supported by this browser';
    }

    hide_overlay();
    
    const _dir_list = async function(path, dir_handle) {
      let res = {};
      if(g_error !== null) return res;
      
      let entries_async = dir_handle.entries();
      let entries = await _Array_fromAsync(entries_async);
      for (let i = 0; i < entries.length; i++) {
        let [name, handle] = entries[i];

        if (handle.kind === 'file') {
          try {
            // NOTE: This is a wasteful way of getting the file size, but there's no easy alternative. See:
            //   https://github.com/WICG/file-system-access/issues/253
            // Reading the whole file into memory is mitigated by the fact that we will likely access it
            // later and it will be in the filesystem cache.
            let file = await handle.getFile();
            res[path+name] = {
              kind: 'file',
              size: file.size,
              time: file.lastModified / 1000
            };
          } catch (err) {
            console.warn(`Failed to read file "${name}": ${err.message}`);
          }
        } else if (handle.kind === 'directory') {
          res[path+name] = {
            kind: 'directory'
          };
          let subres = await _dir_list(path+name+'/', handle);
          Object.assign(res, subres) // merges listings
        }
      }
      return res;
    };

    g_cached_listing = await _dir_list('', g_directory_handle);
   
    let status = {
      path: g_directory_handle === null ? null : g_directory_handle.name,
      list: g_cached_listing,
      error: g_error
    };

    Shiny.setInputValue(status_input_id, status, { priority: 'event' });
    return (status);
  };

  const _Array_fromAsync = async function(asyncIterator){ 
    const arr=[]; 
    for await(const i of asyncIterator) arr.push(i); 
    return arr;
  };
  
  const write = async function(file_path, contents_b64, offset){
    let status = {offset:offset, error: g_error};
    if(status.error !== null) return status;
    
    // show_overlay({message: "Writing file..."}); // TODO: Consider reintroducing as non-visible element to avoid flashing for every action
    
    try { // NOTE: Follows the same logic as #isoaxo
      let path_components = file_path.split('/');
      let fname = path_components.pop();
      let folder_path = path_components.join('/');

      let contents = await _base64_to_buffer(contents_b64);
    
      // 0 - ensure folder exists
      let dir_handle = null;
      if(folder_path === ".") dir_handle = g_directory_handle;
      else{
        // NOTE: Directory creation only handles immediate, singly nested subfolders
        // TODO: Consider before repurposing this file system abstraction layer
        dir_handle = await g_directory_handle.getDirectoryHandle(folder_path, {create: true});
      }
    
      // 1 - write to temp file to the same folder, because we rely on `file.move` to place the file on its Final Destination
      let temp_fname = fname + "_" + crypto.randomUUID() + ".tmp";
      let temp_handle = await dir_handle.getFileHandle(temp_fname, {create: true});
     
      let orig_contents = new ArrayBuffer(0);
      try{
        let orig_file_handle = await dir_handle.getFileHandle(fname, {create: false});
        let orig_file = await orig_file_handle.getFile();
        orig_contents = await orig_file.arrayBuffer();
      } catch(unexamined_error){ /* original file does not exist; nothing to copy */ }
      
      let writable = await temp_handle.createWritable();
      await writable.write(orig_contents);
      await writable.close();
      
      // 2 - get file size
      let file_size = (await temp_handle.getFile()).size;
     
      // 3 - patch and check offset
      if(offset == FS_WRITE_OFFSET_APPEND){
        offset = file_size; // append without checking offset
      }
    
      if(offset > file_size)
        throw new Error(`Write operation to offset "${offset}" would create a hole in '"${file_path}"'.`);
      
      // 4 - compare known cached contents to current contents
      let cached_contents = new ArrayBuffer(0);
      if(g_cached_contents[file_path] !== undefined) cached_contents = g_cached_contents[file_path];
      
      let range_contents_old = cached_contents.slice(offset, offset+contents.byteLength);
      let range_contents_new = orig_contents.slice(offset, offset+contents.byteLength);
      
      let ranges_are_identical = (indexedDB.cmp(range_contents_new, range_contents_old) === 0); // NOTE: From https://stackoverflow.com/a/76795132
      if(!ranges_are_identical){
        throw new Error(`Write operation to '${file_path}' would overwrite contents of unknown origin.`);
      }
      
      // 5 - write proper
      writable = await temp_handle.createWritable({keepExistingData:true});
      writable.seek(offset)
      await writable.write(contents);
      await writable.close();
      
      // 6 - overwrite target file with temp contents
      await temp_handle.move(dir_handle, fname);
      
      let file = await temp_handle.getFile(); // FIXME? Wasteful - could approximate with timestamp and file_size + contents + offset
      let mtime = file.lastModified / 1000;
      file_size = file.size;
      
      // 7 - Update local cached contents
      let start = offset;
      let end = offset+contents.byteLength;
      if(cached_contents.byteLength < end){
        cached_contents = cached_contents.transfer(end); // resize/realloc
      }
      
      let cached_contents_view = new Uint8Array(cached_contents);
      cached_contents_view.set(new Uint8Array(contents), start);
      g_cached_contents[file_path] = cached_contents;
      
      // 8 - Update local cached listing 
      g_cached_listing[file_path] = {kind:'file', size:file_size, time:mtime};
    } catch(error) {
      g_error = status.error = `Error writing ${file_path}: ${error}`;
      console.error(error);
    }

    status.offset = offset;
    
    // hide_overlay(); // TODO: Consider reintroducing as non-visible element to avoid flashing for every action
    return status;
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

  const read = async function({status_input_id, paths}){
    let status = {contents:{}, error: g_error};
    if(g_error !== null) return status;

    show_overlay({message: "Reading files..."});

    try {
      // NOTE: Logic repeats in #thaegh
      let invalid_paths = new Set(paths).difference(new Set(Object.keys(g_cached_listing)));
      if(invalid_paths.size > 0)
        throw new Error("Paths " + Array.from(invalid_paths).join(', ') + " are invalid.");
      
      for(let path of paths){
        let path_components = path.split('/');
        let fname = path_components.pop();
        let dir_handle = g_directory_handle;
        
        for(subdirname of path_components) dir_handle = await dir_handle.getDirectoryHandle(subdirname);
        let file_handle = await dir_handle.getFileHandle(fname);

        let expected_size_in_bytes = g_cached_listing[path].size;
        let file = await file_handle.getFile();

        if (file.size != expected_size_in_bytes)
          throw new Error(`Expected ${expected_size_in_bytes} bytes and got ${file.size} instead.`);
        
        let contents = await file.arrayBuffer();
        g_cached_contents[path] = contents;

        status['contents'][path] = await _buffer_to_base64(contents);
      }
    } catch(error) {
      g_error = status.error = `Error writing: ${path}/${fname}: ${error}`;
      status.contents = {};
      console.error(status.error);
    }

    hide_overlay();

    let t0 = Date.now()
    Shiny.setInputValue(status_input_id, status, {priority: 'event'});
    let t1 = Date.now()
    console.log('Time in setInputValue: '+(t1-t0)/1000)

    return status;
  };

  const execute_IO_plan = async function({status_input_id, plan}) {
    for (let idx=0; idx<plan.length; idx++) {
      let entry = plan[idx];
      if(entry.kind === "write") {
        let status = await write(entry.path, entry.contents, entry.offset);
        plan[idx].error = status.error;
        plan[idx].offset = status.offset; // Patched inside write if entry.offset == FS_WRITE_OFFSET_APPEND
        plan[idx].mtime = g_cached_listing[entry.path].time;
        plan[idx].contents = null;
      } else {
        entry.error = g_error = `Unknown IO action kind: ${entry.kind}`;
        console.error(entry.error);
      }
    }

    const res = { status: plan };
    Shiny.setInputValue(status_input_id, res, {priority: 'event'});
  }

  const init = function() {    
    Shiny.addCustomMessageHandler('dv_fsa_list', list);
    Shiny.addCustomMessageHandler('dv_fsa_read', read);
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
    list: list,
    read: read,
  };

  return (res);
})();

dv_fsa.init();

// Listeners cannot call the picker for security reasons,
// Therefore we must attach manually after a user click
// Possible if a database is available
