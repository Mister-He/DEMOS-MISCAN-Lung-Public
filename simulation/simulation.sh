#!/bin/bash
# ==============================================================================#
# Complete Parallel Execution Script
# ==============================================================================#

# Get the absolute path of the script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="$(dirname "${SCRIPT_DIR}")"
LOG_DIR="${PROJECT_DIR}/outputs/logs"
PID_DIR="${PROJECT_DIR}/outputs/pids"

# Default Configuration
DEFAULT_NODES=60
DEFAULT_START_SEED=1
DEFAULT_END_SEED=20
DEFAULT_STRATEGIES=271
DEFAULT_START_POLICY=0
DEFAULT_END_POLICY=4

# ==============================================================================#
# Function: Run a single node
# ==============================================================================#
run_single() {
    local NODE_ID=$1
    local TOTAL_NODES=$2
    local START_SEED=$3
    local END_SEED=$4
    local STRATEGIES=$5
    local START_POLICY=$6
    local END_POLICY=$7
    
    # Ensure directories exist
    mkdir -p "${LOG_DIR}"

    # Allocate how many scenarios per node
    local SEED_COUNT=$((END_SEED - START_SEED + 1))
    local POLICY_COUNT=$((END_POLICY - START_POLICY + 1))
    local STRATEGY_COUNT=$((STRATEGIES))
    local TOTAL_SCENARIOS=$((SEED_COUNT * POLICY_COUNT * STRATEGY_COUNT))
    local SCENARIOS_PER_NODE=$((TOTAL_SCENARIOS / TOTAL_NODES))

    local THIS_START_SCENARIO=$(( (NODE_ID - 1) * SCENARIOS_PER_NODE + 1 ))
    if [ ${NODE_ID} -eq ${TOTAL_NODES} ]; then
        THIS_END_SCENARIO=$(( TOTAL_SCENARIOS ))
    else
        THIS_END_SCENARIO=$(( NODE_ID * SCENARIOS_PER_NODE ))
    fi
    local THIS_SCENARIO_LIST=$(seq ${THIS_START_SCENARIO} ${THIS_END_SCENARIO})

    # Create node log
    local NODE_LOG="${LOG_DIR}/node_${NODE_ID}_master.log"
    
    {
        echo "========================================================================"
        echo "Node ${NODE_ID} / ${TOTAL_NODES}"
        echo "Running scenarios ${THIS_START_SCENARIO} to ${THIS_END_SCENARIO} (Total: $((THIS_END_SCENARIO - THIS_START_SCENARIO + 1)))"
        echo "Started at: $(date)"
        echo "========================================================================"
    } | tee "${NODE_LOG}"

    # Counter for progress tracking
    local CURRENT=0

    # Change to project tracking directory to run Rscript
    cd "${SCRIPT_DIR}" || exit 1

    # Loop through assigned scenarios
    for SCENARIO_NO in ${THIS_SCENARIO_LIST}; do
        # Allocate seed, policy and strategy for this specific scenario
        local THIS_SEED=$(( (SCENARIO_NO - 1) % SEED_COUNT + START_SEED ))
        local THIS_POLICY=$(( ( (SCENARIO_NO - 1) / SEED_COUNT ) % POLICY_COUNT + START_POLICY ))
        local THIS_STRATEGY=$(( ( (SCENARIO_NO - 1) / (SEED_COUNT * POLICY_COUNT) ) + 1 ))

        CURRENT=$((CURRENT + 1))
        local PERCENT=$((CURRENT * 100 / (THIS_END_SCENARIO - THIS_START_SCENARIO + 1)))

        echo "Node ${NODE_ID}: Seed ${THIS_SEED}, Policy ${THIS_POLICY} Strategy ${THIS_STRATEGY} - ${CURRENT}/$((THIS_END_SCENARIO - THIS_START_SCENARIO + 1)) (${PERCENT}%)" >> "${NODE_LOG}"
        local START_TIME=$(date +%s)

        # Feed parameters to Rscript
        Rscript main.R ${NODE_ID} ${THIS_SEED} ${THIS_STRATEGY} ${THIS_POLICY} 2>&1
	sleep 2

        local END_TIME=$(date +%s)
        echo "|     🕰️  Time: $((END_TIME - START_TIME)) seconds     |" >> "${NODE_LOG}"

        if [ ${PIPESTATUS[0]} -ne 0 ]; then
            echo "ERROR: Strategy ${THIS_STRATEGY} Seed ${THIS_SEED} Policy ${THIS_POLICY} failed!" | tee -a "${NODE_LOG}"
        fi
    done
    
    echo "Node ${NODE_ID} completed at $(date)" | tee -a "${NODE_LOG}"
}

# ==============================================================================#
# Function: Launch all nodes in parallel
# ==============================================================================#
launch_parallel() {
    local TOTAL_NODES=$1
    local START_SEED=$2
    local END_SEED=$3
    local STRATEGIES=$4
    local START_POLICY=$5
    local END_POLICY=$6
    
    mkdir -p "${LOG_DIR}"
    mkdir -p "${PID_DIR}"
    rm -f "${PID_DIR}"/*.pid
    
    echo "Launching ${TOTAL_NODES} nodes (Seeds ${START_SEED}-${END_SEED}, Strategies ${STRATEGIES}, Policies (${START_POLICY}-${END_POLICY}))"
    
    # Export variables for subshells
    export LOG_DIR PID_DIR SCRIPT_DIR PROJECT_DIR
    
    for NODE_ID in $(seq 1 ${TOTAL_NODES}); do
        # Run in background using nohup
        nohup bash -c "$(declare -f run_single); run_single ${NODE_ID} ${TOTAL_NODES} ${START_SEED} ${END_SEED} ${STRATEGIES} ${START_POLICY} ${END_POLICY}" \
            > "${LOG_DIR}/node_${NODE_ID}_nohup.log" 2>&1 &
        
        echo $! > "${PID_DIR}/node_${NODE_ID}.pid"
    done
    
    echo "All nodes launched. Logs in ${LOG_DIR}"
}

# ==============================================================================#
# Function: Check status
# ==============================================================================#
check_status() {
    local RUNNING=0
    local COMPLETED=0
    
    for PID_FILE in "${PID_DIR}"/node_*.pid; do
        [ -f "$PID_FILE" ] || continue
        local NODE_ID=$(basename "$PID_FILE" .pid | sed 's/node_//')
        local PID=$(cat "$PID_FILE")
        
        if ps -p $PID > /dev/null 2>&1; then
            echo "Node ${NODE_ID}: RUNNING (PID: $PID)"
            RUNNING=$((RUNNING + 1))
        else
            echo "Node ${NODE_ID}: COMPLETED"
            COMPLETED=$((COMPLETED + 1))
        fi
    done
    echo "Summary: ${RUNNING} running, ${COMPLETED} completed"
}

# ==============================================================================#
# Function: Stop all
# ==============================================================================#
stop_all() {
    for PID_FILE in "${PID_DIR}"/node_*.pid; do
        [ -f "$PID_FILE" ] || continue
        local PID=$(cat "$PID_FILE")
        if ps -p $PID > /dev/null 2>&1; then
            kill $PID
        fi
        rm -f "$PID_FILE"
    done
    echo "All nodes stopped."
}

# ==============================================================================#
# Main Entry Point
# ==============================================================================#

COMMAND=$1
shift

case "${COMMAND}" in
    "run_parallel"|"run")
        NODES=$DEFAULT_NODES
        START=$DEFAULT_START_SEED
        END=$DEFAULT_END_SEED
        STRATS=$DEFAULT_STRATEGIES
        START_POLICY=$DEFAULT_START_POLICY
        END_POLICY=$DEFAULT_END_POLICY
        
        while [[ $# -gt 0 ]]; do
            case $1 in
                -n|--nodes) NODES="$2"; shift 2 ;;
                -s|--start_seed) START="$2"; shift 2 ;;
                -e|--end_seed) END="$2"; shift 2 ;;
                -st|--strategies) STRATS="$2"; shift 2 ;;
                -p|--start_policy) START_POLICY="$2"; shift 2 ;;
                -P|--end_policy) END_POLICY="$2"; shift 2 ;;
                *) echo "Unknown option: $1"; exit 1 ;;
            esac
        done
        
        launch_parallel "$NODES" "$START" "$END" "$STRATS" "$START_POLICY" "$END_POLICY"
        ;;
    
    "run_single")
        STRATEGY_ID=""
        CORES=1
        SEEDS=1
        POLICY=1
        
        while [[ $# -gt 0 ]]; do
            case $1 in
                -i|--id) STRATEGY_ID="$2"; shift 2 ;;
                -c|--cores) CORES="$2"; shift 2 ;;
                -s|--seeds) SEEDS="$2"; shift 2 ;;
                -p|--policy) POLICY="$2"; shift 2 ;;
                *) echo "Unknown option: $1"; exit 1 ;;
            esac
        done
        
        if [ -z "${STRATEGY_ID}" ]; then
            echo "Error: Strategy ID required (-i)"
            exit 1
        fi
        
        mkdir -p "${LOG_DIR}"
        echo "Running Strategy ${STRATEGY_ID} (${SEEDS} seeds, Policy ${POLICY}) on ${CORES} cores..."
        
        BATCH_SIZE=$(( (SEEDS + CORES - 1) / CORES ))
        
        for i in $(seq 1 ${CORES}); do
            START=$(( (i - 1) * BATCH_SIZE + 1 ))
            END=$(( i * BATCH_SIZE ))
            [ $END -gt $SEEDS ] && END=$SEEDS
            
            if [ $START -le $END ]; then
                (
                    for SEED in $(seq ${START} ${END}); do
                        # Pass 0-based policy index
                        Rscript main.R 1 ${SEED} ${STRATEGY_ID} $((POLICY-1)) > "${LOG_DIR}/strat_${STRATEGY_ID}_seed_${SEED}.log" 2>&1
                    done
                ) &
            fi
        done
        wait
        echo "Completed."
        ;;

    "run_hpc_node")
        # Wrapper to expose the internal run_single function to HPC schedulers like PBS
        NODES=$DEFAULT_NODES
        START=$DEFAULT_START_SEED
        END=$DEFAULT_END_SEED
        STRATS=$DEFAULT_STRATEGIES
        START_POLICY=$DEFAULT_START_POLICY
        END_POLICY=$DEFAULT_END_POLICY
        CURRENT_NODE_ID=1

        while [[ $# -gt 0 ]]; do
            case $1 in
                -i|--id) CURRENT_NODE_ID="$2"; shift 2 ;;
                -n|--nodes) NODES="$2"; shift 2 ;;
                -s|--start_seed) START="$2"; shift 2 ;;
                -e|--end_seed) END="$2"; shift 2 ;;
                -st|--strategies) STRATS="$2"; shift 2 ;;
                -p|--start_policy) START_POLICY="$2"; shift 2 ;;
                -P|--end_policy) END_POLICY="$2"; shift 2 ;;
                *) echo "Unknown option: $1"; exit 1 ;;
            esac
        done

        # Re-use the existing logic to calculate ranges and execute
        run_single "${CURRENT_NODE_ID}" "${NODES}" "${START}" "${END}" "${STRATS}" "${START_POLICY}" "${END_POLICY}"
        ;;

    "status")
        check_status
        ;;
    
    "stop")
        stop_all
        ;;
        
    *)
        echo "Usage: $0 {run|run_single|status|stop} [options]"
        echo "  run -n 20 -s 1 -e 20 -st 271 -p 0 -P 4"
        echo "  run_single -i 100 -c 4 -s 10 -p 1"
        exit 1
        ;;
esac
