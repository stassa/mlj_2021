# Script to run experiments on Linux with bash
#
# The bash version of this script runs all scripts concurrently - debug output
# will be a bit confusing. 

# Root file of the run_experiments.pl module relative to the project root.
experiment_module_root="./experiments/scripts"
# Name of the Prolog module file that defines experiment running predicates.
experiment_module="run_experiments.pl"

# Switch to experiment module root.
# Comment out if you are already running this script from $script_rood.
cd $experiment_module_root

# Run experiments varying user-defined metarules:
swipl -s $experiment_module -g 'run_mtg_fragment(acc,10,0.5,[])' -t halt &
# Sleep two seconds to give logging the chance to create a new log file.
# Log files are timestamped to the second.
sleep 2
swipl -s $experiment_module -g 'run_mtg_fragment(time,10,0.5,[])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_robots(acc,10,0.5,[])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_robots(time,10,0.5,[])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(acc,10,0.5,[])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(time,10,0.5,[])' -t halt &
sleep 2

# Run experiments complementing user-defined with metarules learned by matrix
# metarule specialisation:
swipl -s $experiment_module -g 'run_mtg_fragment(acc,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_mtg_fragment(time,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_robots(acc,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_robots(time,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(acc,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(time,10,0.5,[meta_monadic,meta_dyadic])' -t halt &
sleep 2

# Run experiments complementing user-defined with metarules learned by punch
# metarule specialisation:
swipl -s $experiment_module -g 'run_mtg_fragment(acc,10,0.5,[higher_order(3,3)])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_mtg_fragment(time,10,0.5,[higher_order(3,3)])' -t halt &
# higher_order(2,3) generates too many metarules and makes mtg_fragment.pl take
# a very, Very, VERY long time when measuring accuracy.
sleep 2
swipl -s $experiment_module -g 'run_robots(acc,10,0.5,[higher_order(2,3)])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_robots(time,10,0.5,[higher_order(2,3)])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(acc,10,0.5,[higher_order(2,3)])' -t halt &
sleep 2
swipl -s $experiment_module -g 'run_coloured_graph(time,10,0.5,[higher_order(2,3)])' -t halt &

# NOTE: Experiments can take a long time to run. If you only want to test that
# every test runs correctly and generates the expected output comment out the
# lines running each expeiment above and uncomment the following lines.
#
#swipl -s $experiment_module -g 'run_mtg_fragment(acc,2,1,[])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_mtg_fragment(time,2,1,[])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(acc,2,1,[])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(time,2,1,[])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(acc,2,1,[])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(time,2,1,[])' -t halt &
#sleep 2
#
#swipl -s $experiment_module -g 'run_mtg_fragment(acc,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_mtg_fragment(time,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(acc,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(time,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(acc,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(time,2,1,[meta_monadic,meta_dyadic])' -t halt &
#sleep 2
#
#swipl -s $experiment_module -g 'run_mtg_fragment(acc,2,1,[higher_order(2,3)])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_mtg_fragment(time,2,1,[higher_order(2,3)])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(acc,2,1,[higher_order(2,3)])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_robots(time,2,1,[higher_order(2,3)])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(acc,2,1,[higher_order(2,3)])' -t halt &
#sleep 2
#swipl -s $experiment_module -g 'run_coloured_graph(time,2,1,[higher_order(2,3)])' -t halt &

# Return to project root
cd ../..
