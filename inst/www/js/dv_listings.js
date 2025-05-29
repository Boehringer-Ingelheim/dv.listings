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
        let result = '';
        let options = choices;
        result += `<select style=\"width:100%%\" onchange=\"Shiny.setInputValue('${id}', {row:${row[row_number_idx]}, option:this.value}, {priority: 'event'});\">`;
        for (let i = 0; i < options.length; i += 1) {
          result += `<option value=${i + 1}${options[i] == data ? ' selected' : ''}>${options[i]}</option>`;
        }
        result += '</select>';

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

  const res = {
    review_column_render: review_column_render,
    render_selection: render_selection,
    render_identity: render_identity,
    render_status: render_status,
    show_child: show_child
  }
  return (res)
})()