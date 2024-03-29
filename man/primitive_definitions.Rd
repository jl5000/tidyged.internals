% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docs_primitives.R
\name{primitive_definitions}
\alias{primitive_definitions}
\title{Define parameters in GEDCOM 5.5.5 specification}
\arguments{
\item{character_encoding}{A code value that represents the character set and encoding to be used to
interpret this data. Allowed values: "UTF-8" and "UNICODE" (UTF-16).}

\item{gedcom_form}{A value that identifies the GEDCOM form used in this GEDCOM file. The value must be
alphanumerical string. This string is case-sensitive. Only "LINEAGE-LINKED" is supported.}

\item{gedcom_version_number}{The version number of the specification used.}

\item{address_city}{The name of the city/town used in the address.}

\item{address_country}{The name of the country that pertains to the associated address.}

\item{address_email}{An electronic address that can be used for contact such as an email address.}

\item{address_fax}{A FAX telephone number appropriate for sending data facsimiles.}

\item{local_address_lines}{The local address lines before the town/city. This can be a vector with up to 3 elements.}

\item{address_postal_code}{The ZIP or postal code used by the various localities in handling of mail.}

\item{address_state}{The name of the province, state or similar country subdivision used in the address.}

\item{address_web_page}{The world wide web page address.}

\item{adopted_by_which_parent}{A code which shows which parent in the associated family group record adopted this person.
Use "HUSB" for husband, "WIFE" for wife, or "BOTH" for both.}

\item{age_at_event}{A string that indicates the age in years, months, and days that the
principal was at the time of the associated event. See Details.}

\item{husband_age_at_event}{A string that indicates the age in years, months, and days that the
husband was at the time of the associated event. See Details.}

\item{wife_age_at_event}{A string that indicates the age in years, months, and days that the
wife was at the time of the associated event. See Details.}

\item{attribute_descriptor}{Text describing a particular characteristic or attribute assigned to an individual.}

\item{attribute_type}{An attribute which may have caused name, addresses, phone numbers,
family listings to be recorded. Its application is in helping to classify sources used for information.}

\item{automated_record_id}{A unique record identification number assigned to the record by
the source system. This number is intended to serve as a more sure means of identification
of a record for reconciling differences in data between two interfacing systems.}

\item{caste_name}{A name assigned to a particular group that this person was associated with,
such as a particular racial group, religious group, or a group with an inherited status.}

\item{cause_of_event}{Used in special cases to record the reasons which precipitated an event.
Normally this will be used subordinate to a death event to show cause of death, such as
might be listed on a death certificate.}

\item{certainty_assessment}{A quantitative evaluation of the credibility of a piece of
information, based upon its supporting evidence. Some systems use this feature to rank multiple
conflicting opinions for display of most likely information first. It is not intended to
eliminate the receiver's need to evaluate the evidence for themselves.
0 = unreliable/estimated data
1 = Questionable reliability of evidence
2 = Secondary evidence, data officially recorded sometime after event
3 = Direct and primary evidence used, or by dominance of the evidence}

\item{copyright_gedcom_file}{A copyright statement needed to protect the copyrights of the
submitter of this GEDCOM file.}

\item{copyright_source_data}{A copyright statement required by the owner of data from
which this information was downloaded.}

\item{count_of_children}{The known number of children of this individual from all marriages or,
if subordinate to a family group record, the reported number of children known to belong to this
family, regardless of whether the associated children are represented in the corresponding
structure. This is not necessarily the count of children listed in a family structure.}

\item{descriptive_title}{The title of a work, record, item, or object.}

\item{entry_recording_date}{A date_calendar(), date_period(), date_range(), or date_approximated() value
giving the date that this event data was entered into the original source document.}

\item{event_attribute_type}{A code that classifies the principal event or happening that caused the
source record entry to be created. If the event or attribute doesn't translate to one of these tag codes,
then a user supplied value is expected and will be generally classified in the category of other.}

\item{event_descriptor}{Text describing a particular event pertaining to the individual or family.
This event value is usually assigned to the EVEN tag. The classification as to the difference
between this specific event and other occurrences of the EVENt tag is indicated by the use of
a subordinate TYPE tag selected from the EVENT_DETAIL structure.}

\item{event_or_fact_classification}{A descriptive word or phrase used to further classify the parent
event or attribute tag. This should be used whenever either of the generic EVEN or FACT tags are used.
The value of this primitive is responsible for classifying the generic event or fact being cited.}

\item{event_type_cited_from}{A code that indicates the type of event which was responsible for the
source entry being recorded. For example, if the entry was created to record a birth of a child,
then the type would be BIRT regardless of the assertions made from that record, such as the mother's
name or mother's birth date. This will allow a prioritised best view choice and a determination of
the certainty associated with the source used in asserting the cited fact.}

\item{event_type_family}{A code used to indicate the type of family event. One of:
"ANUL", "CENS", "DIV", "DIVF", "ENGA", "MARB", "MARC", "MARR", "MARL", "MARS", "RESI", "EVEN".}

\item{event_type_individual}{A code used to indicate the type of individual event. One of:
"BIRT", "CHR", "DEAT", "BURI", "CREM", "ADOP", "BAPM", "BARM", "BASM", "CHRA", "CONF", "FCOM",
"NATU", "EMIG", "IMMI", "CENS", "PROB", "WILL", "GRAD", "RETI", "EVEN".}

\item{events_recorded}{An enumeration of the different kinds of events that were recorded in a
particular source. Each enumeration is separated by a comma. Such as a parish register of births,
deaths, and marriages would be BIRT, DEAT, MARR.}

\item{file_creation_date}{A date_exact() object giving the date that this file was created.}

\item{gedcom_content_description}{A note that a user enters to describe the contents of the
lineage-linked file in terms of "ancestors or descendants of" so that the person receiving
the data knows what genealogical information the file contains.}

\item{gedcom_file_name}{The name of the GEDCOM file. A GEDCOM file name should use the format
basename.ext, and use the file extension .GED (or .ged).}

\item{id_number}{A third-party number assigned to an individual.}

\item{language_of_text}{The human language in which the data in the file is normally read or
written. It is used primarily by programs to select language-specific sorting sequences and phonetic
name matching algorithms.}

\item{multimedia_file_reference}{A complete local or remote file reference to the auxiliary data
to be linked to the GEDCOM context. Remote reference would include a network address where the multimedia
data may be obtained.}

\item{multimedia_format}{Indicates the format of the multimedia data associated with the specific
GEDCOM context. This allows processors to determine whether they can process the data object.
Any linked files should contain the data required, in the indicated format, to process the file data. One of:
"AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "MKV", "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "WAV".}

\item{name_of_business}{Name of the business, corporation, or person that produced or commissioned the product.}

\item{name_of_product}{The name of the software product that produced this file.}

\item{name_of_repository}{The official name of the archive in which the stated source material is stored.}

\item{name_of_source_data}{The name of the electronic data source that was used to obtain the data in
this file. For example, the data may have been obtained from a CD-ROM disc that was named
"U.S. 1880 CENSUS CD-ROM vol. 13."}

\item{name_personal}{The full name formed in the manner the name is normally spoken.
The surname of an individual, if known, is enclosed between two slash (/) characters.
The order of the name parts should be the order that the person would, by custom of their culture,
have used when giving it to a recorder.}

\item{name_phonetic}{A character vector of phonetic variations of the full name.}

\item{name_piece_given}{Given name or earned name. Different given names are separated by a comma.}

\item{name_piece_nickname}{A descriptive or familiar name used in connection with one's proper name.}

\item{name_piece_prefix}{Non indexing name piece that appears preceding the given name and surname parts.
Different name prefix parts are separated by a comma.}

\item{name_piece_suffix}{Non-indexing name piece that appears after the given name and surname parts.
Different name suffix parts are separated by a comma.}

\item{name_piece_surname}{Surname or family name. Different surnames are separated by a comma.}

\item{name_piece_surname_prefix}{Surname prefix or article used in a family name. A surname prefix that
consists of multiple parts is written as is, and not modified in any way. Thus, the surname prefix for the
surname “de la Cruz” is “de la”.}

\item{name_romanised}{A character vector giving romanised variations of the name.}

\item{name_type}{Indicates the name type, for example the name issued or assumed as an immigrant.}

\item{national_or_tribal_origin}{The person's division of national origin or other folk, house, kindred,
lineage, or tribal interest.}

\item{nobility_type_title}{The title given to or used by a person, especially of royalty or other noble
class within a locality.}

\item{number_of_relationships}{The number of different relationships (family groups) that this person was
known to have been a member of as a partner, regardless of whether the associated relationships are present
in the GEDCOM file.}

\item{occupation}{The kind of activity that an individual does for a job, profession, or principal activity.}

\item{pedigree_linkage_type}{A code used to indicate the child to family relationship for pedigree navigation
purposes. One of "birth", "adopted", "foster".}

\item{phone_number}{A phone number.}

\item{phonetisation_method}{Indicates the method used in transforming the text to the phonetic variation.}

\item{physical_description}{An unstructured list of the attributes that describe the physical
characteristics of a person, place, or object. Commas separate each attribute. For example:
Hair Brown, Eyes Brown, Height 5 ft 8 in.}

\item{place_latitude}{The value specifying the latitudinal coordinate of the place name.
The latitude coordinate is the direction North or South from the equator in degrees and fraction of
degrees carried out to give the desired accuracy. For example: 18 degrees, 9 minutes, and 3.4 seconds North
would be formatted as N18.150944.}

\item{place_longitude}{The value specifying the longitudinal coordinate of the place name.
The longitude coordinate is Degrees and fraction of degrees east or west of the zero or base
meridian coordinate. For example: 168 degrees, 9 minutes, and 3.4 seconds East would be formatted as E168.150944.}

\item{place_name}{The jurisdictional name of the place where the event took place.
Jurisdictions are separated by commas. No part of the place name may be replaced by an abbreviation.
Place names are not terminated by a full stop or anything else.}

\item{place_phonetic}{A character vector of phonetic variations of the place name.}

\item{place_romanised}{A character vector of romanised variations of the place name.}

\item{possessions}{A list of possessions (real estate or other property) belonging to this individual.}

\item{product_version_number}{The version of the product that created the GEDCOM file.
It is defined and changed by the creators of the product.}

\item{publication_date}{A date_exact() object giving the date this source was published or created.}

\item{receiving_system_name}{The name of the system expected to process the GEDCOM file.}

\item{relation_is_descriptor}{A word or phrase that states object 1's relation is object 2.}

\item{religious_affiliation}{A name of the religion with which this person, event, or record was affiliated.}

\item{responsible_agency}{The organization, institution, corporation, person, or other entity that has
responsibility for the associated context. For example, an employer of a person of an associated occupation,
or a church that administered rites or events, or an organization responsible for creating and/or archiving
records.}

\item{role_descriptor}{A word or phrase that identifies a person's role in an event being described.
This should be the same word or phrase, and in the same language, that the recorder used to define
the role in the actual record.}

\item{role_in_event}{Indicates what role this person played in the event that is being cited in this context.}

\item{romanisation_method}{Indicates the method used in transforming the text to a romanised variation.}

\item{scholastic_achievement}{A description of a scholastic or educational achievement or pursuit.}

\item{sex_value}{A code that indicates the sex of the individual. Either "M" (male), "F" (female), "U" (undetermined),
"X" (intersex), or "N" (not recorded).}

\item{source_call_number}{An identification or reference description used to file and retrieve items
from the holdings of a repository.}

\item{source_descriptive_title}{The title of the work, record, or item and, when appropriate,
the title of the larger work or series of which it is a part.}

\item{source_filed_by_entry}{This entry is to provide a short title used for sorting, filing,
and retrieving source records.}

\item{source_jurisdiction_place}{The name of the lowest jurisdiction that encompasses all lower-level places
named in this source.}

\item{source_media_type}{A media classification code that indicates the type of material in which
the referenced source is stored. One of: "audio", "book", "card", "electronic", "fiche", "film",
"magazine", "manuscript", "map", "newspaper", "photo", "tombstone", "video".}

\item{source_originator}{The person, agency, or entity who created the record. For a published work,
this could be the author, compiler, transcriber, abstractor, or editor. For an unpublished source,
this may be an individual, a government agency, church organization, or private organization, etc.}

\item{source_publication_facts}{When and where the record was created. For published works,
this includes information such as the city of publication, name of the publisher, and year of publication.}

\item{submitter_name}{The name of the submitter formatted for display and address generation.}

\item{system_id}{A system identification name. This name must be unique for each system (product),
different from any other system. The name may include spaces, and is not restricted to ASCII characters.}

\item{text_from_source}{A verbatim copy of any description contained within the source.
This indicates notes or text that are actually contained in the source document, not the submitter's
opinion about the source.}

\item{time_value}{The time of a specific event.}

\item{file_creation_time}{The time at which the file was created.}

\item{user_reference_number}{A user-defined number or text that the submitter uses to identify this record.}

\item{user_reference_type}{A user-defined definition of the user_reference_number.}

\item{user_text}{Free-form user text. Comments, opinions.}

\item{where_within_source}{Specific location within the information referenced.}

\item{xref_fam}{An xref ID of a Family Group record.}

\item{xref_indi}{An xref ID of an Individual record.}

\item{xref_sour}{An xref ID of a Source record.}

\item{xref_repo}{An xref ID of a Repository record.}

\item{xref_obje}{An xref ID of a Multimedia record.}

\item{xref_note}{An xref ID of a Note record.}

\item{xref_subm}{An xref ID of a Submitter record.}

\item{date_changed}{A CHANGE_DATE() object giving the time this record was last modified. If not provided,
the current date is used.}

\item{notes}{A list of NOTE_STRUCTURE() objects.}

\item{source_citations}{A list of SOURCE_CITATION() objects.}

\item{source_repository_citations}{A list of SOURCE_REPOSITORY_CITATION() objects.}

\item{multimedia_links}{A list of MULTIMEDIA_LINK() objects}
}
\description{
Define all primitives used in GEDCOM records and subrecords.
}
\details{
This empty function serves as a single location where all primitives are defined, mainly for
efficiency and maintainability. The first values defined are given earlier in the specification and not
with the rest of the primitives.
}
