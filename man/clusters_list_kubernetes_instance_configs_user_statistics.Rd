% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generated_client.R
\name{clusters_list_kubernetes_instance_configs_user_statistics}
\alias{clusters_list_kubernetes_instance_configs_user_statistics}
\title{Get statistics about the current users of an Instance Config}
\usage{
clusters_list_kubernetes_instance_configs_user_statistics(
  instance_config_id,
  order = NULL,
  order_dir = NULL
)
}
\arguments{
\item{instance_config_id}{integer required. The ID of this instance config.}

\item{order}{string optional. The field on which to order the result set. Defaults to running_deployments. Must be one of pending_memory_requested, pending_cpu_requested, running_memory_requested, running_cpu_requested, pending_deployments, running_deployments.}

\item{order_dir}{string optional. Direction in which to sort, either asc (ascending) or desc (descending). Defaults to desc.}
}
\value{
An array containing the following fields:
\item{userId}{string, The owning user's ID}
\item{userName}{string, The owning user's name}
\item{pendingDeployments}{integer, The number of deployments belonging to the owning user in "pending" state}
\item{pendingMemoryRequested}{integer, The sum of memory requests (in MB) for deployments belonging to the owning user in "pending" state}
\item{pendingCpuRequested}{integer, The sum of CPU requests (in millicores) for deployments belonging to the owning user in "pending" state}
\item{runningDeployments}{integer, The number of deployments belonging to the owning user in "running" state}
\item{runningMemoryRequested}{integer, The sum of memory requests (in MB) for deployments belonging to the owning user in "running" state}
\item{runningCpuRequested}{integer, The sum of CPU requests (in millicores) for deployments belonging to the owning user in "running" state}
}
\description{
Get statistics about the current users of an Instance Config
}
