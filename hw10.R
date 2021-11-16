library(RedditExtractoR)
library(dplyr)

# Creates a dataframe of threads from search results for "Patch 11 notes" 
# over the past year within the League of Legends subreddit
PatchNoteThreads <- find_thread_urls(keywords="Patch 11 notes", subreddit="leagueoflegends", sort_by="relevance", period="year")

# Filters threads by containing the term 'Patch" in title
FilteredPatchNotes <- PatchNoteThreads %>% filter(grepl('Patch', title))

# Obtains thread information from search results
PatchNotesContent <- get_thread_content(FilteredPatchNotes$url)

# Displays comments in a 2d matrix along with author, thread title, etc
PatchNotesComments <- PatchNotesContent[["comments"]]

PatchNotesComments


