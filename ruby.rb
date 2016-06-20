##
# Controller for Admin User actions:
# - User Dashboard
# - Filter Dashboard
# - Report Dashboard
class AdminToolsController < ApplicationController

  before_action :authenticate_user!

  include SmartListing::Helper::ControllerExtensions
  helper SmartListing::Helper

  ##
  # Display a table of all system Users, with details such as their login count, if they are online or not, etc.
  #
  # Currently, does not provide any actions, other than the ability to add a new User.
  #
  # Requires the current User to have :administer User privileges
  def user_dashboard
    authorize! :administer, User

    @users = smart_listing_create(
      :users,
      User.all,
      partial: "user_listing",
      sort_attributes: [
        [:id, "users.id"],
        [:email, "users.email"],
        [:role, "users.role"],
        [:last_seen, "users.last_seen"],
        [:sign_in_count, "users.sign_in_count"],
        [:current_sign_in_ip, "users.current_sign_in_ip"],
        [:updated_at, "users.updated_at"],
        ],
        default_sort: { updated: "desc" }
        )
  end

  ##
  # Display a table of all Filters on the system.
  #
  # Actions available include edit and delete
  #
  # Requires the current User to have :administer Filter privileges.
  def filter_dashboard
    authorize! :administer, Filter

    @filters = smart_listing_create(
      :all_filters,
      Filter.all.joins(:user),
      partial: "filters/listing",
      sort_attributes: [
        [:title,      "filters.title"],
        [:description,    "filters.description"],
        [:type,       "filters.filter_type"],
        [:cdm_user_count, "filters.cdm_user_count"],
        [:author,     "users.email"],
        [:created,      "filters.created_at"],
        [:updated,      "filters.updated_at"]
        ],
        default_sort: { updated: "desc" }
        )
  end

  ##
  # Display a table of all Reports on the system.
  #
  # Actions available include view, download, edit, share and delete
  #
  # Requires the current User to have :administer Report privileges.
  def report_dashboard
    authorize! :administer, Report

    @reports = smart_listing_create(
      :all_reports,
      Report.all.joins(:user),
      partial: "reports/listing",
      sort_attributes: [
        [:title,    "reports.title"],
        [:description,  "reports.description"],
        [:author,   "users.email"],
        [:created,    "reports.created_at"],
        [:updated,    "reports.updated_at"]
        ],
        default_sort: { updated: "desc" }
        )
  end
end
