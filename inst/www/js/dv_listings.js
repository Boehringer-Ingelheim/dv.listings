const dv_listings = (function() {

  const render_HTML_review = function(json_string) {
    let html = '';
    let data;
    try {
      data = typeof json_string === 'string' ? JSON.parse(json_string) : json_string;
    } catch (e) {
      return '<em>Invalid JSON</em>';
    }
  
    const reviews = data.reviews;
    if (!reviews) return '<em>No reviews</em>';
  
    function isEmptyObject(obj) {
      return obj && typeof obj === 'object' && Object.keys(obj).length === 0;
    }
  
    Object.entries(reviews).forEach(([key, r]) => {
      const role = r.role || key;
      const reviewText = (typeof r.review === 'string')
        ? r.review
        : (isEmptyObject(r.review) ? 'Not reviewed' : JSON.stringify(r.review));
      const timestamp = (!isEmptyObject(r.timestamp) && !isNaN(r.timestamp))
        ? new Date(r.timestamp * 1000).toLocaleString()
        : 'No timestamp';
      const reviewedLabel = r.reviewed_at_least_once
        ? '<span class="label label-success">Reviewed</span>'
        : '<span class="label label-warning">Pending</span>';
  
      html += `
        <div class="review-render" style="display: flex; align-items: center; gap: 10px;">
          <strong>${role}</strong>
          ${reviewedLabel}
          <small style="flex: 1 1 auto;">${reviewText}</small>
          <em class="text-muted" style="margin-left: auto;">${timestamp}</em>
        </div>
        <hr/>
      `;
    });
  
    return html;
  } 
  
  const review_column_render = function(data, type, row, meta){     
    return render_HTML_review(data);
  }

  const res = {
    review_column_render: review_column_render
  }
  return(res)
})()