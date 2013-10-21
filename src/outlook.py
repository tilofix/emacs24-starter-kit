"""
Module outlook provides access to Outlook via win32com.client from PyWin32.

Module outlook provides access to Outlook with help of PyWin32 and defines
  - a class wrapping an outlook application and
    provides methods to access outlook types and resources.
  - a main() function being called with options and arguments parsed in
    from command line.
  - a private _test() function to prepare running of doctests.

Python documentation of this module can be called with command:
  - "pydoc" or
  - "C:\\Python25\\Lib\\pydoc.py"
Command line option "-w" creates a local <module_name>.html documentation.

The module as well as function documentation is compliant with epydoc.
It uses the Epytext syntax. Epydoc creates a subdirectory "html"
containing the generated documentation.
Run epydoc command with option "-v" to see syntax errors.
"""

import optparse

try:
    import Pymacs
    import win32com.client
except ImportError:
    import sys
    sys.stderr.write("Missing python modules!\n")
    sys.exit(1)

"""
If you need to use constants like win32com.client.constants.olFolderContacts
from Outlook type library 'msoutl.olb' run following command and
select "Microsoft Outlook 12.0 Object Library (9.3)" from list poping up.
  c:/Python25/Lib/site-packages/win32com/client $ python makepy.py
  Generating to c:\Python25\lib\site-packages\win32com\gen_py\00062FFF-0000-0000-C000-000000000046x0x9x3.py
  Building definitions from type library...
  Generating...
  Importing module

New Outlook "Microsoft Outlook 14.0 Object Library (9.4)".
"""

class Outlook:

    def __init__(self):
        """
        Creates Outlook Application
        """
        self.objOutlk = win32com.client.Dispatch("Outlook.Application")
        self.objNSpace = self.objOutlk.GetNamespace("MAPI")


    def createMailItemHtml(self, a_html_body):
        """
        Creates a new Outlook mail item and inserts @param a_html_body as body.

        @param a_html_body: Content of a html document
        @type  a_html_body: str

        @return: Nothing
        @rtype: None

        >>> Outlook().createMailItemHtml("<html><head></head><body><h1>Outlook</h1></body></html>")
        """
        # olMailItem | 0 | Represents a MailItem
        item = self.objOutlk.CreateItem(win32com.client.constants.olMailItem)
        """ Item newly created by 
        CreateItem(self, ItemType=defaultNamedNotOptArg)
        """
        # olFormatHTML | 2 | HTML format
        item.BodyFormat = win32com.client.constants.olFormatHTML
        item.HTMLBody = a_html_body
        item.Display()

    def displayItemByID(self, a_id_str):
        """
        Displays an Email in Outlook by EntryID.
        
        >>> Outlook().displayItemByID("00000000DE8C7A6152813B4D8FB8478C4FB88CDF64202A00")
        """
        item = self.objNSpace.GetItemFromID(a_id_str)
        """ Item fetched by 
        GetItemFromID(self, EntryIDItem=defaultNamedNotOptArg, EntryIDStore=defaultNamedOptArg)
        """
        if item:
            item.Display()

    
def publish_html_to_outlook_mail(a_html_buf):
    """
    Publish a buffer containing HTML to a new outlook mail item.
    """
    Pymacs.lisp.switch_to_buffer(a_html_buf)
    buf_content = Pymacs.lisp.buffer_string()
    my_outlook = Outlook()
    my_outlook.createMailItemHtml(buf_content)
    

# publish_html_to_outlook_mail.interaction = ''


def display_outlook_item(a_item_id):
    """
    """
    my_outlook = Outlook()
    my_outlook.displayItemByID(a_item_id)

# display_outlook_item.interaction = ''


def _test(a_opts, a_args):
    """
    Function "_test" is called with command line option "-t" or "--run-test".
    """
    import doctest, outlook
  
    return doctest.testmod(outlook, verbose=True)


def main(a_opts, a_args):
    """
    Function main prints options and arguments.

    Function takes
      - a optparse.Value instance containing all the options, 
      - a list of arguments, and
        prints each option with its name and value and each argument.

    @param a_opts: Options from command line.
    @type  a_opts: optparse.Values

    @param a_args: Arguments from command line.
    @type  a_args: list of str

    @return: Execution result.
    @rtype: str
    
    >>> from optparse import Values as value
    >>> main(value({'opt_do_tests':'False',
    ...             'opt_your_option':'Your Option',
    ...             'opt_verbose':True}),
    ...            ['arg1','arg2'])
    opt_verbose:True
    opt_your_option:Your Option
    opt_do_tests:False
    arg1
    arg2
    'Done'

    """
    opts_dict = a_opts.__dict__
    for key in opts_dict:
        print key+":"+str(opts_dict[key])
    for arg in a_args:
        print arg
    # ...
    return "Done"

if __name__ == "__main__":
    usage_msg = "usage: %prog [options] ARGS"
    parser = optparse.OptionParser(usage=usage_msg)
    # Most actions tell optparse to store a value in some variable -
    # for example, take a string from the command line and
    # store it in an attribute of options. 
    # If you don't specify an option action, optparse defaults to store. 
    # If you don't specify a type, optparse assumes string.
    parser.add_option("-t", "--run-tests", 
                      action="store_true",
                      dest="opt_do_tests",
                      default=False,
                      help="Start tests of this module", metavar="__file__")
    parser.add_option("-o", "--an-option", 
                      action="store",
                      type="string",
                      dest="opt_an_option",
                      default='Your Option',
                      help="This could be your option OPT", metavar="OPT")
    parser.add_option("-v", "--verbose",
                      action="store_true", 
                      dest="opt_verbose", 
                      default=False,
                      help="Print messages to stdout")
    options, args = parser.parse_args()

    if options.opt_do_tests:
        _test(options, args)
    else:
        main(options, args)

