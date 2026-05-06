#!/bin/bash
# filepath: /Users/yichenhe/Desktop/Work/MyDEMOS/simulation/psa.sh

set -euo pipefail

# ==============================================================================
# Parallel Batch Processing Script with Status Checking
# ==============================================================================

# Default values
TOTAL_TASKS=100
NUM_CORES=5
POLICY_IDX=0
OUTPUT_DIR="../outputs/lc_psa_output"

LOG_DIR="${OUTPUT_DIR}/logs"
PROGRESS_DIR="${OUTPUT_DIR}/.progress"
STATUS_FILE="${OUTPUT_DIR}/.status"
PID_FILE="${OUTPUT_DIR}/.pids"

# ==============================================================================
# Function: Show help
# ==============================================================================
show_help() {
  cat << EOF
Usage: $0 [OPTIONS]

Options:
  -n, --tasks NUM     Total number of tasks (default: 100)
  -c, --cores NUM     Number of cores to use (default: 5)
  -p, --policy IDX    Policy index (default: 0)
  -o, --output DIR    Output directory (default: ../outputs/lc_psa_output)
  -s, --status        Show current status of running jobs
  -k, --kill          Kill all running jobs
  -h, --help          Show this help message

Examples:
  # Start new batch processing
  ./psa.sh -n 100 -c 5 -p 0

  # Check status
  ./psa.sh -s

  # Kill all jobs
  ./psa.sh -k
EOF
}

# ==============================================================================
# Function: Update progress file
# ==============================================================================
update_progress() {
  local core_id="$1"
  local current_task="$2"
  local total_tasks="$3"
  local status="$4"

  local progress_file="${PROGRESS_DIR}/core_${core_id}.progress"
  echo "${current_task}|${total_tasks}|${status}|$(date +%s)" > "${progress_file}"
}

# ==============================================================================
# Function: Generate summary
# ==============================================================================
generate_summary() {
  local status="$1"

  if [ ! -f "${STATUS_FILE}" ]; then
    return
  fi

  # shellcheck disable=SC1090
  source "${STATUS_FILE}"

  local END_TIME
  END_TIME=$(date +%s)
  local DURATION=$(( END_TIME - JOB_START_TIME ))
  local HOURS=$(( DURATION / 3600 ))
  local MINUTES=$(( (DURATION % 3600) / 60 ))
  local SECONDS=$(( DURATION % 60 ))

  local SUMMARY_FILE="${LOG_DIR}/summary.txt"
  {
    echo "================================================================"
    echo "Batch Processing Summary - ${status}"
    echo "================================================================"
    echo "Job ID: ${JOB_ID}"
    echo "Start time: ${START_TIME_STR}"
    echo "End time: $(date)"
    echo "Duration: ${HOURS}h ${MINUTES}m ${SECONDS}s"
    echo ""
    echo "Configuration:"
    echo "  Total tasks: ${JOB_TOTAL_TASKS}"
    echo "  Number of cores: ${JOB_NUM_CORES}"
    echo "  Policy Index: ${JOB_POLICY_IDX}"
    echo ""
    echo "Results by core:"

    local grand_total_success=0
    local grand_total_failed=0

    for core in $(seq 1 "${JOB_NUM_CORES}"); do
      local log_file="${LOG_DIR}/core_${core}.log"
      if [ -f "${log_file}" ]; then
        echo ""
        echo "  Core ${core}:"
        echo "    Log: ${log_file}"

        local completed failed
        completed=$(grep -c "completed successfully" "${log_file}" 2>/dev/null || true)
        failed=$(grep -c "failed with exit code" "${log_file}" 2>/dev/null || true)

        completed=${completed:-0}
        failed=${failed:-0}

        echo "    Completed: ${completed}"
        echo "    Failed: ${failed}"

        local total_attempts=$(( completed + failed ))
        if [ "${total_attempts}" -gt 0 ]; then
          local success_rate=$(( completed * 100 / total_attempts ))
          echo "    Success rate: ${success_rate}%"
        else
          echo "    Success rate: N/A"
        fi

        grand_total_success=$(( grand_total_success + completed ))
        grand_total_failed=$(( grand_total_failed + failed ))
      fi
    done

    echo ""
    echo "----------------------------------------------------------------"
    echo "Grand Total:"
    echo "  Successfully completed: ${grand_total_success}"
    echo "  Failed: ${grand_total_failed}"

    local grand_total=$(( grand_total_success + grand_total_failed ))
    if [ "${grand_total}" -gt 0 ]; then
      local overall_success_rate=$(( grand_total_success * 100 / grand_total ))
      echo "  Overall success rate: ${overall_success_rate}%"
    else
      echo "  Overall success rate: N/A"
    fi
    echo "================================================================"
  } > "${SUMMARY_FILE}"

  cat "${SUMMARY_FILE}"
}

# ==============================================================================
# Function: Run batch on a single core
# ==============================================================================
run_batch() {
  local core_id="$1"
  local start_idx="$2"
  local end_idx="$3"
  local policy_idx="$4"
  local log_file="${LOG_DIR}/core_${core_id}.log"

  : > "${log_file}"

  {
    echo "================================================================"
    echo "Core ${core_id} - Started at $(date)"
    echo "Task range: ${start_idx} to ${end_idx}"
    echo "Policy Index: ${policy_idx}"
    echo "================================================================"
    echo ""

    local total_tasks=$(( end_idx - start_idx + 1 ))
    local completed=0
    local failed=0

    for i in $(seq "${start_idx}" "${end_idx}"); do
      completed=$(( completed + 1 ))

      update_progress "${core_id}" "${completed}" "${total_tasks}" "running (policy ${policy_idx})"
      echo "----------------------------------------------------------------"

      if Rscript main.R 1 ${i} 1 ${policy_idx} 2>&1; then
        echo "✓ Task ${i} completed successfully"
        update_progress "${core_id}" "${completed}" "${total_tasks}" "success"
      else
        local exit_code=$?
        echo "✗ Task ${i} failed with exit code ${exit_code}"
        failed=$(( failed + 1 ))
        update_progress "${core_id}" "${completed}" "${total_tasks}" "failed"
      fi

      echo ""
    done

    echo "================================================================"
    echo "Core ${core_id} - Completed at $(date)"
    echo "Tasks completed: ${completed}"
    echo "Tasks failed: ${failed}"

    if [ "${completed}" -gt 0 ]; then
      local success_rate=$(( (completed - failed) * 100 / completed ))
      echo "Success rate: ${success_rate}%"
    else
      echo "Success rate: N/A"
    fi
    echo "================================================================"

  } >> "${log_file}" 2>&1

  update_progress "${core_id}" "${total_tasks}" "${total_tasks}" "complete"
}

# ==============================================================================
# Function: Show status
# ==============================================================================
show_status() {
  if [ ! -f "${STATUS_FILE}" ]; then
    echo "No running jobs found."
    echo ""
    echo "Start a new job with: ./psa.sh -n <tasks> -c <cores>"
    exit 0
  fi

  # shellcheck disable=SC1090
  source "${STATUS_FILE}"

  echo "================================================================"
  echo "Job Status - $(date '+%Y-%m-%d %H:%M:%S')"
  echo "================================================================"
  echo "Job ID: ${JOB_ID}"
  echo "Started at: ${START_TIME_STR}"
  echo "Total tasks: ${JOB_TOTAL_TASKS}"
  echo "Policy Index: ${JOB_POLICY_IDX}"
  echo "Number of cores: ${JOB_NUM_CORES}"
  echo ""

  local current_time elapsed elapsed_hours elapsed_minutes elapsed_seconds
  current_time=$(date +%s)
  elapsed=$(( current_time - JOB_START_TIME ))
  elapsed_hours=$(( elapsed / 3600 ))
  elapsed_minutes=$(( (elapsed % 3600) / 60 ))
  elapsed_seconds=$(( elapsed % 60 ))
  echo "Elapsed time: ${elapsed_hours}h ${elapsed_minutes}m ${elapsed_seconds}s"
  echo ""

  local running_count=0
  if [ -f "${PID_FILE}" ]; then
    while read -r pid; do
      if kill -0 "${pid}" 2>/dev/null; then
        running_count=$(( running_count + 1 ))
      fi
    done < "${PID_FILE}"
  fi

  echo "Running processes: ${running_count}/${JOB_NUM_CORES}"
  echo ""
  echo "----------------------------------------------------------------"
  echo "Progress by Core:"
  echo "----------------------------------------------------------------"

  local total_completed=0
  local total_failed=0
  local all_complete=true

  for core in $(seq 1 "${JOB_NUM_CORES}"); do
    local progress_file="${PROGRESS_DIR}/core_${core}.progress"

    if [ -f "${progress_file}" ]; then
      local current total status timestamp
      IFS='|' read -r current total status timestamp < "${progress_file}"

      current=${current:-0}
      total=${total:-1}
      status=${status:-running}

      local status_icon="⚙ "
      local status_text="Running"
      case "${status}" in
        success)
          status_icon="✓ "
          status_text="Running"
          ;;
        failed)
          status_icon="✗ "
          status_text="Failed"
          ;;
        complete)
          status_icon="✓ "
          status_text="Complete"
          ;;
      esac

      if [ "${status}" != "complete" ]; then
        all_complete=false
      fi

      printf "Core %d: %s%-10s %d/%d tasks\n" \
        "${core}" "${status_icon}" "${status_text}" "${current}" "${total}"

      total_completed=$(( total_completed + current ))

      local log_file="${LOG_DIR}/core_${core}.log"
      if [ -f "${log_file}" ]; then
        local failed_count
        failed_count=$(grep -c "failed with exit code" "${log_file}" 2>/dev/null || true)
        failed_count=${failed_count:-0}
        total_failed=$(( total_failed + failed_count ))
      fi
    else
      echo "Core ${core}: Waiting to start..."
      all_complete=false
    fi
  done

  echo ""
  echo "----------------------------------------------------------------"
  echo "Overall Progress: ${total_completed}/${JOB_TOTAL_TASKS} tasks"
  if [ "${total_failed}" -gt 0 ]; then
    echo "Failed tasks: ${total_failed}"
  fi

  if [ "${total_completed}" -gt 0 ] && [ "${elapsed}" -gt 0 ]; then
    local avg_time_per_task remaining_tasks estimated_remaining est_hours est_minutes est_seconds
    avg_time_per_task=$(( elapsed / total_completed ))
    remaining_tasks=$(( JOB_TOTAL_TASKS - total_completed ))

    if [ "${remaining_tasks}" -gt 0 ] && [ "${avg_time_per_task}" -gt 0 ]; then
      estimated_remaining=$(( remaining_tasks * avg_time_per_task ))
      est_hours=$(( estimated_remaining / 3600 ))
      est_minutes=$(( (estimated_remaining % 3600) / 60 ))
      est_seconds=$(( estimated_remaining % 60 ))

      echo "Estimated time remaining: ${est_hours}h ${est_minutes}m ${est_seconds}s"
    fi
  fi

  echo "----------------------------------------------------------------"
  echo ""

  if [ "${all_complete}" = true ] && [ "${running_count}" -eq 0 ]; then
    echo "✓ All jobs completed!"
    echo ""
    echo "View summary: cat ${LOG_DIR}/summary.txt"
    echo "View logs: ls ${LOG_DIR}/"
  else
    echo "Jobs are still running in background."
    echo "Check status again with: ./psa.sh -s"
    echo "Kill all jobs with: ./psa.sh -k"
  fi
  echo ""
}

# ==============================================================================
# Function: Kill all jobs
# ==============================================================================
kill_jobs() {
  if [ ! -f "${PID_FILE}" ]; then
    echo "No running jobs found."
    exit 0
  fi

  echo "Killing all background jobs..."

  local killed_count=0
  while read -r pid; do
    if kill -0 "${pid}" 2>/dev/null; then
      echo "  Killing PID ${pid}"
      kill -TERM "${pid}" 2>/dev/null || true
      killed_count=$(( killed_count + 1 ))
    fi
  done < "${PID_FILE}"

  sleep 2

  while read -r pid; do
    if kill -0 "${pid}" 2>/dev/null; then
      echo "  Force killing PID ${pid}"
      kill -9 "${pid}" 2>/dev/null || true
    fi
  done < "${PID_FILE}"

  pkill -9 -f "main.R" 2>/dev/null || true

  echo ""
  echo "Killed ${killed_count} processes."

  generate_summary "INTERRUPTED"

  rm -f "${PID_FILE}"
  rm -f "${STATUS_FILE}"
  rm -rf "${PROGRESS_DIR}"

  echo "Cleanup completed."
}

# ==============================================================================
# Function: Start background jobs
# ==============================================================================
start_jobs() {
  if [ -f "${STATUS_FILE}" ]; then
    echo "Warning: There seems to be a job already running."
    echo "Check status with: ./psa.sh -s"
    echo "Kill existing job with: ./psa.sh -k"
    echo ""
    read -r -p "Start a new job anyway? (y/n) " -n 1 reply
    echo ""
    if [[ ! "${reply}" =~ ^[Yy]$ ]]; then
      exit 0
    fi
    rm -f "${STATUS_FILE}" "${PID_FILE}"
  fi

  mkdir -p "${OUTPUT_DIR}" "${LOG_DIR}" "${PROGRESS_DIR}"

  rm -f "${PROGRESS_DIR}"/core_*.progress || true
  rm -f "${PID_FILE}" || true

  local TASKS_PER_CORE=$(( (TOTAL_TASKS + NUM_CORES - 1) / NUM_CORES ))

  echo "================================================================"
  echo "Starting Batch Processing in Background"
  echo "Policy Index: ${POLICY_IDX}"
  echo "================================================================"
  echo "Total tasks: ${TOTAL_TASKS}"
  echo "Number of cores: ${NUM_CORES}"
  echo "Tasks per core: ~${TASKS_PER_CORE}"
  echo "Output directory: ${OUTPUT_DIR}"
  echo "Log directory: ${LOG_DIR}"
  echo "================================================================"
  echo ""

  local JOB_ID="job_$(date +%Y%m%d_%H%M%S)"
  local START_TIME
  START_TIME=$(date +%s)
  local START_TIME_STR
  START_TIME_STR="$(date)"

  cat > "${STATUS_FILE}" << EOF
JOB_ID="${JOB_ID}"
JOB_POLICY_IDX="${POLICY_IDX}"
JOB_START_TIME="${START_TIME}"
START_TIME_STR="${START_TIME_STR}"
JOB_TOTAL_TASKS="${TOTAL_TASKS}"
JOB_NUM_CORES="${NUM_CORES}"
EOF

  for core in $(seq 1 "${NUM_CORES}"); do
    local start_idx=$(( (core - 1) * TASKS_PER_CORE + 1 ))
    local end_idx=$(( core * TASKS_PER_CORE ))

    if [ "${core}" -eq "${NUM_CORES}" ]; then
      end_idx="${TOTAL_TASKS}"
    fi

    if [ "${start_idx}" -le "${TOTAL_TASKS}" ]; then
      echo "Launching Core ${core}: tasks ${start_idx}-${end_idx}"
      run_batch "${core}" "${start_idx}" "${end_idx}" "${POLICY_IDX}" &
      echo $! >> "${PID_FILE}"
    fi
  done

  echo ""
  echo "✓ All ${NUM_CORES} cores launched in background!"
  echo ""
  echo "Job ID: ${JOB_ID}"
  echo ""
  echo "Commands:"
  echo "  Check status: ./psa.sh -s"
  echo "  Kill all jobs: ./psa.sh -k"
  echo "  View logs: tail -f ${LOG_DIR}/core_1.log"
  echo ""

  (
    while true; do
      sleep 10

      all_done=true
      if [ -f "${PID_FILE}" ]; then
        while read -r pid; do
          if kill -0 "${pid}" 2>/dev/null; then
            all_done=false
            break
          fi
        done < "${PID_FILE}"
      fi

      if [ "${all_done}" = true ]; then
        generate_summary "COMPLETED"
        rm -f "${PID_FILE}" "${STATUS_FILE}"
        rm -rf "${PROGRESS_DIR}"
        break
      fi
    done
  ) &

  disown -a
}

# ==============================================================================
# Parse command-line arguments
# ==============================================================================
ACTION="start"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--tasks)
      TOTAL_TASKS="$2"
      shift 2
      ;;
    -c|--cores)
      NUM_CORES="$2"
      shift 2
      ;;
    -p|--policy)
      POLICY_IDX="$2"
      shift 2
      ;;
    -o|--output)
      OUTPUT_DIR="$2"
      LOG_DIR="${OUTPUT_DIR}/logs"
      PROGRESS_DIR="${OUTPUT_DIR}/.progress"
      STATUS_FILE="${OUTPUT_DIR}/.status"
      PID_FILE="${OUTPUT_DIR}/.pids"
      shift 2
      ;;
    -s|--status)
      ACTION="status"
      shift
      ;;
    -k|--kill)
      ACTION="kill"
      shift
      ;;
    -h|--help)
      show_help
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use -h or --help for usage information"
      exit 1
      ;;
  esac
done

# ==============================================================================
# Execute action
# ==============================================================================
case "${ACTION}" in
  start)
    if [ "${TOTAL_TASKS}" -lt 1 ]; then
      echo "Error: Total tasks must be at least 1"
      exit 1
    fi

    if [ "${NUM_CORES}" -lt 1 ]; then
      echo "Error: Number of cores must be at least 1"
      exit 1
    fi

    start_jobs
    ;;
  status)
    show_status
    ;;
  kill)
    kill_jobs
    ;;
esac