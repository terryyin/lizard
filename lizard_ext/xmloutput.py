class CppNcssXMLFormatter(object):

    def xml_output(self, result, options):
        '''
        Thanks for Holy Wen from Nokia Siemens Networks to let me use his code
        to put the result into xml file that is compatible with cppncss.
        Jenkens has plugin for cppncss format result to display the diagram.
        '''
        import xml.dom.minidom

        impl = xml.dom.minidom.getDOMImplementation()
        doc = impl.createDocument(None, "cppncss", None)
        root = doc.documentElement

        pi = doc.createProcessingInstruction(
            'xml-stylesheet',
            'type="text/xsl" ' +
            'href="https://raw.github.com/terryyin/lizard/master/lizard.xsl"')
        doc.insertBefore(pi, root)

        measure = doc.createElement("measure")
        measure.setAttribute("type", "Function")
        measure.appendChild(self._createLabels(doc, ["Nr.", "NCSS", "CCN"]))

        Nr = 0
        total_func_ncss = 0
        total_func_ccn = 0

        for source_file in result:
            file_name = source_file.filename
            for func in source_file.function_list:
                Nr += 1
                total_func_ncss += func.nloc
                total_func_ccn += func.cyclomatic_complexity
                measure.appendChild(
                    self._createFunctionItem(
                        doc, Nr, file_name, func, options.verbose))

            if Nr != 0:
                measure.appendChild(
                    self._createLabeledValueItem(
                        doc, 'average', "NCSS", str(total_func_ncss / Nr)))
                measure.appendChild(
                    self._createLabeledValueItem(
                        doc, 'average', "CCN", str(total_func_ccn / Nr)))

        root.appendChild(measure)

        measure = doc.createElement("measure")
        measure.setAttribute("type", "File")
        measure.appendChild(
            self._createLabels(doc, ["Nr.", "NCSS", "CCN", "Functions"]))

        file_NR = 0
        file_total_ncss = 0
        file_total_ccn = 0
        file_total_funcs = 0

        for source_file in result:
            file_NR += 1
            file_total_ncss += source_file.nloc
            file_total_ccn += source_file.CCN
            file_total_funcs += len(source_file.function_list)
            measure.appendChild(
                self._createFileNode(doc, source_file, file_NR))

        if file_NR != 0:
            fileSummary = [("NCSS", file_total_ncss / file_NR),
                           ("CCN", file_total_ccn / file_NR),
                           ("Functions", file_total_funcs / file_NR)]
            for k, v in fileSummary:
                measure.appendChild(
                    self._createLabeledValueItem(doc, 'average', k, v))

        summary = [("NCSS", file_total_ncss),
                   ("CCN", file_total_ccn),
                   ("Functions", file_total_funcs)]
        for k, v in summary:
            measure.appendChild(self._createLabeledValueItem(doc, 'sum', k, v))

        root.appendChild(measure)

        xmlString = doc.toprettyxml()
        return xmlString

    def _createLabel(self, doc, name):
        label = doc.createElement("label")
        text1 = doc.createTextNode(name)
        label.appendChild(text1)
        return label

    def _createLabels(self, doc, labelNames):
        labels = doc.createElement("labels")
        for label in labelNames:
            labels.appendChild(self._createLabel(doc, label))

        return labels

    def _createFunctionItem(self, doc, Nr, file_name, func, verbose):
        item = doc.createElement("item")
        if verbose:
            item.setAttribute(
                "name", "%s at %s:%s" %
                (func.long_name, file_name, func.start_line))
        else:
            item.setAttribute(
                "name", "%s(...) at %s:%s" %
                (func.name, file_name, func.start_line))
        value1 = doc.createElement("value")
        text1 = doc.createTextNode(str(Nr))
        value1.appendChild(text1)
        item.appendChild(value1)
        value2 = doc.createElement("value")
        text2 = doc.createTextNode(str(func.nloc))
        value2.appendChild(text2)
        item.appendChild(value2)
        value3 = doc.createElement("value")
        text3 = doc.createTextNode(str(func.cyclomatic_complexity))
        value3.appendChild(text3)
        item.appendChild(value3)
        return item

    def _createLabeledValueItem(self, doc, name, label, value):
        average_ncss = doc.createElement(name)
        average_ncss.setAttribute("lable", label)
        average_ncss.setAttribute("value", str(value))
        return average_ncss

    def _createFileNode(self, doc, source_file, file_NR):
        item = doc.createElement("item")
        item.setAttribute("name", source_file.filename)
        value1 = doc.createElement("value")
        text1 = doc.createTextNode(str(file_NR))
        value1.appendChild(text1)
        item.appendChild(value1)
        value2 = doc.createElement("value")
        text2 = doc.createTextNode(str(source_file.nloc))
        value2.appendChild(text2)
        item.appendChild(value2)
        value3 = doc.createElement("value")
        text3 = doc.createTextNode(str(source_file.CCN))
        value3.appendChild(text3)
        item.appendChild(value3)
        value4 = doc.createElement("value")
        text4 = doc.createTextNode(str(len(source_file.function_list)))
        value4.appendChild(text4)
        item.appendChild(value4)
        return item

