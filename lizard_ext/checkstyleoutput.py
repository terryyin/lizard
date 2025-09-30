'''
Checkstyle XML output for Lizard
'''


def checkstyle_output(all_result, verbose):
    result = all_result.result
    import xml.etree.ElementTree as ET

    checkstyle = ET.Element('checkstyle', version="4.3")

    for source_file in result:
        if source_file:
            file_elem = ET.SubElement(checkstyle, 'file', name=source_file.filename)
            for func in source_file.function_list:
                # Each function with a warning (e.g., CCN > threshold) could be an <error>
                # For now, output all functions as <error> for demonstration
                ET.SubElement(
                    file_elem,
                    'error',
                    line=str(func.start_line),
                    column="0",
                    severity="info",
                    message=f"{func.name} has {func.nloc} NLOC, {func.cyclomatic_complexity} CCN, "
                           f"{func.token_count} token, {len(func.parameters)} PARAM, {func.length} length",
                    source="lizard"
                )

    # Pretty print
    import xml.dom.minidom
    rough_string = ET.tostring(checkstyle, 'utf-8')
    reparsed = xml.dom.minidom.parseString(rough_string)
    return reparsed.toprettyxml(indent="  ")
