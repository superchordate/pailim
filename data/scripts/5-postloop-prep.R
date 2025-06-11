# Manipulate data based on Carly's request
if(!cache.ok(5)){

    # ===== PALESTINE DATA PREPARATION =====
    pa %<>% 
        mutate(
            `Palestine/Israel` = 'Palestine',
            Month = factor(Month, levels = 1:12, labels = month.abb),
            Perpetrator.Origin = fifelse(
                !is.na(Perpetrator.Origin.2) | !is.na(Perpetrator.Origin.3),
                'Multiple Perpetrators',
                Perpetrator.Origin.1
            ),
            Victim.Type = fifelse(
                !is.na(Victim.2) | (Victim.1 == 'Multiple Victim'),
                'Multiple Victims',
                Victim.1
            ),
            # Recode perpetrator origin
            Perpetrator.Origin = fifelse(Perpetrator.Origin == 'Foreign', "Abroad", Perpetrator.Origin),
            # Recode victim types
            Victim.Type = fcase(
                Victim.Type == 'Civilian', "Israeli Civilian",
                Victim.Type == 'Military', "Israeli Military", 
                Victim.Type == 'Government', "Israeli Government",
                default = Victim.Type
            ),
            Riot.SubCategory = fifelse(
                !is.na(Riot.SubCategory.2) | !is.na(Riot.SubCategory.3) |
                !is.na(Riot.SubCategory.4) | !is.na(Riot.SubCategory.5) | 
                !is.na(Riot.SubCategory.6),
                'Multiple Types',
                Riot.SubCategory.1
            ),
            Riot.SubCategory = fcase(
                Riot.SubCategory == 'Border/Blockage Breaching', 'Border/Blockade Breaching',
                Riot.SubCategory == 'Stones Incendiary', 'Stones and Incendiary',
                Riot.SubCategory == 'Stone', 'Stones',
                Riot.SubCategory == '', NA_character_,
                default = Riot.SubCategory
            )
        )

    # ===== ISRAEL DATA PREPARATION =====
    il %<>%
        mutate(
            Month = factor(Month, levels = 1:12, labels = month.abb),
            `Palestine/Israel` = 'Israel',
            Perpetrator.Type = fifelse(!is.na(Perpetrator.2), 'Multiple Perpetrators', Perpetrator.1),
            Victim.Type = fifelse(
                !is.na(Victim.2) | !is.na(Victim.3) | !is.na(Victim.4),
                'Multiple Victims',
                Victim.1
            ),
            # Recode perpetrator and victim types
            Perpetrator.Type = fifelse(Perpetrator.Type == 'Civilians', "Israeli Civilians", Perpetrator.Type),
            Victim.Type = fcase(
                Victim.Type == 'PCI', "Palestinian Citizen of Israel",
                Victim.Type == 'Palestinian Child', "Palestinian Minor",
                default = Victim.Type
            )
        )

    # ===== DATE PROCESSING =====
    add_date_variables <- function(df) {
        df %>%
            mutate(
                Date = as.Date(as.character(Date), format = "%Y%m%d"),
                MonthNum = lubridate::month(Date),
                Week = lubridate::week(Date),
                Quarter = lubridate::quarter(Date)
            )
    }

    pa <- add_date_variables(pa)
    il <- add_date_variables(il)

    # Reload and clean text data
    clean_text <- function(text) {
        text %>%
            stringr::str_remove_all("[<>+]") %>%
            stringr::str_remove_all("U00..")
    }

    pa %<>% mutate(Verbatim.Report = clean_text(Verbatim.Report))
    il %<>% mutate(Verbatim.Report = clean_text(Verbatim.Report))

    # ===== COMBINE DATASETS =====
    cm = bind_rows(
        pa %>% select(
            Add,Year,Month,Date,Week,MonthNum,Quarter,Longitude,Latitude,Casualties,Killed,Injured,Verbatim.Report,Type.Violence,Israeli.CPI, Palestinian.CPI,Israeli.UE.Quarterly,Palestinian.UE.Quarterly,Israeli.Trade.Balance,Palestinian.Trade.Balance,Exchange.Rate,Demolished.Structures.Daily,TA125.PX_CLOSE,PASISI.PX_CLOSE,
            TAVG,PRCP,Total.Entries.Exits.Gaza.Israel, Total.Imports.Gaza.Israel, Total.Exports.Gaza.Israel,Victim.Type,`Palestine/Israel`,
            starts_with('Secondary.Type.Violence')
        ),
        il %>% select(
            Add,Year,Month,Date,Week,MonthNum,Quarter,Longitude,Latitude,Casualties,Killed,Injured,Verbatim.Report,Type.Violence, Israeli.CPI, Palestinian.CPI,Israeli.UE.Quarterly,Palestinian.UE.Quarterly,Israeli.Trade.Balance,Palestinian.Trade.Balance,Exchange.Rate,Demolished.Structures.Daily,TA125.PX_CLOSE,PASISI.PX_CLOSE,
            TAVG,PRCP,Total.Entries.Exits.Gaza.Israel, Total.Imports.Gaza.Israel, Total.Exports.Gaza.Israel, Victim.Type,`Palestine/Israel`,
            starts_with('Secondary.Type.Violence')
        )
    )
    cm$`Palestine/Israel` = factor(cm$`Palestine/Israel`)

    # ===== FILTER DATA =====
    # Start from 2010 and remove empty cities for Israel data
    pa <- pa %>% filter(Year >= 2010)
    il <- il %>% filter(Year >= 2010, City != "")
    cm <- cm %>% filter(Year >= 2010)

    # Uncomment to sample Israel data if needed
    # il <- il %>% slice_sample(n = 4000) %>% arrange(Date)

    save.cache(pa, il, cm)

}