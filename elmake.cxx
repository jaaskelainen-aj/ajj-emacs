#include <list>
using namespace std;
#include <cpp4scripts.hpp>
using namespace c4s;

program_arguments args;
int main(int argc, char **argv)
{
    path_iterator el;
    args += argument("--help",  false, "Outputs this help / parameter list.");
    try {
        args.initialize(argc,argv);
    }catch(c4s_exception ce){
        cout << "Incorrect parameters.\n"<<ce.what()<<'\n';
        return 1;
    }
    if( args.is_set("--help") ) {
        args.usage();
        return 0;
    }
    try {
        process emacs("emacs");
        emacs.pipe_default();
        path_list els(path("./"), "*.el");
        for(el=els.begin(); el!=els.end(); el++) {
            if(compiled_file(*el, "./", ".elc").outdated()) {
                cout<<el->get_path()<<" >>\n";
                emacs.set_args("-Q --batch -L . -l package -f package-initialize -f batch-byte-compile");
                emacs+=el->get_path();
                emacs();
            }
        }
    }
    catch (const c4s_exception &ce) {
        cout<<"Compile failed:"<<ce.what()<<'\n';
        return 1;
    }
    return 0;
}
