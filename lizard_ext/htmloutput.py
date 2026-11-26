# -*- coding: utf-8 -*-

'''
This module extends the default output formatting to include HTML.
'''

from __future__ import print_function
import sys
import datetime


def html_output(result, options, scheme=None, *_):
    try:
        from jinja2 import Template
    except ImportError:
        sys.stderr.write(
                "HTML Output depends on jinja2. `pip install jinja2` first")
        sys.exit(2)

    from lizard import OutputScheme
    scheme = scheme or OutputScheme(options.extensions)
    core_fields = {'nloc', 'cyclomatic_complexity', 'token_count',
                   'parameter_count', 'length', 'location'}
    extension_columns = [item for item in scheme.items
                         if item['value'] not in core_fields
                         and item.get('caption')]
    file_list = []
    saved_files = []
    for source_file in result:
        saved_files.append(source_file)
        if source_file:
            source_file_dict = {"filename": source_file.filename}
            func_list = []
            for source_function in source_file.function_list:
                if source_function:
                    source_function_dict = _create_dict(source_function)
                    source_function_dict.update(
                        (column['value'], getattr(source_function,
                                                  column['value']))
                        for column in extension_columns)
                    func_list.append(source_function_dict)
                    source_file_dict["functions"] = func_list
        file_list.append(source_file_dict)
    output = Template(TEMPLATE).render(
            title='Lizard code complexity report',
            date=datetime.datetime.now().strftime('%Y-%m-%d %H:%M'),
            thresholds=options.thresholds, files=file_list,
            extension_columns=extension_columns,
            numeric_columns=list(range(2, 6 + len(extension_columns))))
    print(output)
    from lizard import get_warnings
    return len(list(get_warnings(saved_files, options)))


def _create_dict(obj):
    return dict(obj.__dict__, parameter_count=obj.parameter_count)


TEMPLATE = '''<!DOCTYPE HTML PUBLIC
"-//W3C//DTD HTML 4.01 Transitional//EN"
"http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <title>Code complexity report</title>
  <!-- DataTables CSS -->
  <link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.13.4/css/jquery.dataTables.min.css">
  <!-- jQuery -->
  <script src="https://code.jquery.com/jquery-3.7.0.min.js"></script>
  <!-- DataTables JS -->
  <script src="https://cdn.datatables.net/1.13.4/js/jquery.dataTables.min.js"></script>
  <style>
    h2
    {
          text-align: center;
    }
        td.footer
        {
          text-align: center;
          padding-top: 3px;
          font-family: sans-serif;
        }
    td.greater-value
        {
          padding-top: 2px;
          text-align: center;
          padding-left: 10px;
          padding-right: 10px;
          font-family: sans-serif;
          white-space: nowrap;
      background-color: LightPink;
        }
    td.lesser-value
        {
          padding-top: 2px;
          text-align: center;
          padding-left: 10px;
          padding-right: 10px;
          font-family: sans-serif;
          white-space: nowrap;
      background-color: LightGreen;
        }
    td.value
        {
          padding-top: 2px;
          text-align: center;
          padding-left: 10px;
          padding-right: 10px;
          font-family: sans-serif;
          white-space: nowrap;
        }
    td.file-header
        {
          background-color: LightBlue;
          font-weight: bold;
        }
    td.function-name
        {
          background-color: LightSteelBlue;
        }
    /* DataTables wrapper styling */
    .dataTables_wrapper {
          margin: 0 auto;
          width: 95%;
        }
    /* Fallback styling if DataTables CSS doesn't load */
    table#complexityTable {
          border-collapse: collapse;
          width: 95%;
          margin: 0 auto;
        }
    table#complexityTable th {
          background-color: #4CAF50;
          color: white;
          padding: 10px;
          text-align: left;
          border: 1px solid #ddd;
        }
    table#complexityTable td {
          border: 1px solid #ddd;
        }
    table#complexityTable tr:nth-child(even) {
          background-color: #f2f2f2;
        }
    table#complexityTable tr:hover {
          background-color: #ddd;
        }
  </style>
 </head>
 <body>
<h2>Code Complexity Report</h2>

<center>

<table id="complexityTable" class="display" style="width:100%">
  <thead>
    <tr>
      <th>File</th>
      <th>Function name</th>
      <th>Cyclomatic complexity ({{ thresholds["cyclomatic_complexity"] }})</th>
      <th>LOC ({{ thresholds["nloc"] }})</th>
      <th>
        {% if thresholds["token_count"] %}
           Token count ({{ thresholds["token_count"] }})
        {% else %}
           Token count
        {% endif %}
      </th>
      <th>Parameter count ({{ thresholds["parameter_count"] }})</th>
      {% for column in extension_columns %}
      <th>{{ column.caption | trim }}{% if column.value in thresholds %}
      ({{ thresholds[column.value] }}){% endif %}</th>
      {% endfor %}
    </tr>
  </thead>
  <tbody>
{% for file in files %}
  {% for func in file.functions %}
  <tr>
    <td>{{ file.filename }}</td>
    <td class="function-name">{{ func.name }}</td>
    {% if func.cyclomatic_complexity > thresholds["cyclomatic_complexity"] %}
       <td class="greater-value">{{ func.cyclomatic_complexity }}</td>
    {% else %}
       <td class="lesser-value">{{ func.cyclomatic_complexity }}</td>
    {% endif %}

    {% if func.nloc > thresholds["nloc"] %}
       <td class="greater-value">{{ func.nloc }}</td>
    {% else %}
       <td class="lesser-value">{{ func.nloc}}</td>
    {% endif %}

    {% if thresholds["token_count"] %}
       {% if func.token_count > thresholds["token_count"] %}
          <td class="greater-value">{{ func.token_count }}</td>
       {% else %}
          <td class="lesser-value">{{ func.token_count }}</td>
       {% endif %}
    {% else %}
       <td class="value">{{ func.token_count }}</td>
    {% endif %}

    {% if func.parameter_count > thresholds["parameter_count"] %}
       <td class="greater-value">{{ func.parameter_count }}</td>
    {% else %}
       <td class="lesser-value">{{ func.parameter_count }}</td>
    {% endif %}
    {% for column in extension_columns %}
      {% if column.value not in thresholds %}
        <td class="value">{{ func[column.value] }}</td>
      {% elif func[column.value] > thresholds[column.value] %}
        <td class="greater-value">{{ func[column.value] }}</td>
      {% else %}
        <td class="lesser-value">{{ func[column.value] }}</td>
      {% endif %}
    {% endfor %}
  </tr>
  {% endfor %}
{% endfor %}
  </tbody>
</table>
<center>

<br>
<table width="100%" border=0 cellspacing=0 cellpadding=0>
    <tr><td class="footer">Generated by
    <a href="http://www.lizard.ws/">Lizard</a> on {{ date }}
    </td></tr>
</table>

<script>
// Gracefully degrade if CDN is unavailable
if (typeof jQuery !== 'undefined' && typeof jQuery.fn.dataTable !== 'undefined') {
    $(document).ready(function() {
        try {
            $('#complexityTable').DataTable({
                "pageLength": 25,
                "order": [[2, "desc"]],  // Sort by cyclomatic complexity descending by default
                "columnDefs": [
                    { "type": "num", "targets": {{ numeric_columns }} }  // Ensure numeric sorting for metric columns
                ]
            });
        } catch (e) {
            console.warn('DataTables initialization failed. Displaying static table.', e);
        }
    });
} else {
    // Fallback: Table will display as static HTML
    console.info('DataTables not available. Displaying static HTML table.');
}
</script>

 </body>
</html>

'''
