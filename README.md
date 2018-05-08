# Survival Analysis for GCLM

Survival analysis of male/female GCLM wild-type (+/+) or knockout (-/-).

* Censor
    * 1 is exact death/event -- natural death or euthanasia as moribund.
    * 0 is unnatural death -- accident, tissue collection, wide-spread skin infection
    
* uid = unique animal id
* cage = animal cage (grouped housing)
* sex 
    * 0 = Male, 1 = Female
    
* gt - genotype, We are excluding heterozygote as not part of our current project
    * 0 = wild-type (GCLM+/+)
    * 1 = heterozygote (GCLM+/-) 
    * 2 = full knockout (GCLM-/-)
    
* Cause of death
    * 0 = Natural death
    * 1 = Euthanized
    * 2 = Accident
    * 3 = Collected Tissue
    
### Goal of project

Determine lifespan differences between GCLM+/+ (wild-type) and GCLM-/- (knockout) male and female mice.

The two files are identical, just one in Rmarkdown and one as a single .r file.