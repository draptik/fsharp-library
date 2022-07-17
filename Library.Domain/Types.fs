module Library.Domain

open System

// Types -------------------------------------------

type Undefined = exn

type TimeStamp = DateTimeOffset

type Author = { Name: string; }
type Borrower = { Name: string }
type Librarian = { Name: string }
type BookId = int
type Isbn = string

type Info = { Authors: Author list; Title: string; Isbn: Isbn }
type RequestForCheckoutIsbn = Isbn
type RequestForCheckoutByBookId = BookId
type ReturnBookId = BookId
type BookSearch = Info

type Book = { Id: BookId; Info: Info; AddedBy: Librarian; AddedAt: TimeStamp }

type Catalog = Book list

type Circulation = { BookId: BookId; BorrowedBy: Borrower; BorrowedAt: TimeStamp; ReturnedAt: TimeStamp option }
let isCheckedOut circulation = circulation.ReturnedAt = None
let isReturned circulation = not (isCheckedOut circulation)

type Circulations = Circulation list
let areCheckedOut circulations = circulations |> List.filter (fun x -> x |> isCheckedOut)

type LibraryState = {
    Catalog: Catalog
    Circulations: Circulations
}

// Actions / Workflows --------------------------

type AddBookWorkflow = LibraryState -> Librarian -> TimeStamp -> Info -> LibraryState

type CheckoutByIsbnWorkflow = LibraryState -> RequestForCheckoutIsbn -> Borrower -> TimeStamp -> LibraryState
type CheckoutByBookIdWorkflow = LibraryState -> RequestForCheckoutByBookId -> Borrower -> TimeStamp -> LibraryState
type ReturnBookWorkflow = LibraryState -> ReturnBookId -> Borrower -> TimeStamp -> LibraryState


// Implementations ------------------------------

let initialLibraryState = { Catalog = []; Circulations = [] }

let addBookToEmptyCatalogState state info librarian now =
    { state with Catalog = [{ Id = 0; Info = info; AddedBy = librarian; AddedAt = now }] }

let addNewBookToExistingCatalogState state info librarian now =
    let newCatalogEntry = { Id = 0; Info = info; AddedBy = librarian; AddedAt = now }
    { state with Catalog = newCatalogEntry :: state.Catalog }
    
let addBookWorkflow : AddBookWorkflow =
    fun state librarian now info ->
        let catalog = state.Catalog
        match catalog with
        | [] ->
            addBookToEmptyCatalogState state info librarian now
        | books ->
           let booksWithSameIsbn =
               books |> List.filter (fun x -> x.Info.Isbn = info.Isbn)
           match booksWithSameIsbn with
           | [] ->
                addNewBookToExistingCatalogState state info librarian now
           | books' ->
                let newestId = books' |> List.map (fun x -> x.Id) |> List.max
                let nextId = newestId+1
                let newCopyOfBook = { Info = info; Id = nextId; AddedBy = librarian; AddedAt = now }
                { state with Catalog = newCopyOfBook :: catalog }
        
let checkoutByIsbnWorkflow : CheckoutByIsbnWorkflow =
    fun state requestForCheckout borrower now ->
        let catalog = state.Catalog
        let circulations = state.Circulations
        let matchingBooksInCatalog =
            catalog |> List.filter (fun x -> x.Info.Isbn = requestForCheckout)
        
        let maybeAvailableBookIds : BookId list option =
            match matchingBooksInCatalog with
            | [] ->
                None
            | matchingBooks ->
                let matchingBookIds = matchingBooks |> List.map (fun x -> x.Id) |> Set.ofList
                let checkedOutBookIds =
                    circulations |> areCheckedOut |> List.map (fun x -> x.BookId) |> Set.ofList
                Set.difference matchingBookIds checkedOutBookIds |> Set.toList |> Some

        match maybeAvailableBookIds with
        | None ->
            state
        | Some bookIds ->
            let bookId = bookIds[0] // this assumes no parallel checkouts (no async/await)
            let newCheckout = { BorrowedBy = borrower; BorrowedAt = now; BookId = bookId; ReturnedAt = None }
            { state with Circulations = newCheckout :: state.Circulations }

let checkoutByBookIdWorkflow : CheckoutByBookIdWorkflow =
    fun state requestForCheckout borrower now ->
        let catalog = state.Catalog
        let circulations = state.Circulations
        let matchingBookIdsInCatalog =
            catalog |> List.filter (fun x -> x.Id = requestForCheckout) |> List.map (fun x -> x.Id)
        match matchingBookIdsInCatalog with
        | [] -> state
        | bookIdsMatchingRequest ->
            let checkedOutBookIds =
                circulations |> areCheckedOut |> List.map (fun x -> x.BookId) |> Set.ofList
            let bookIdsAvailableForCheckout =
                Set.difference (bookIdsMatchingRequest |> Set.ofList) checkedOutBookIds |> Set.toList
                
            match bookIdsAvailableForCheckout with
            | [] ->
                state
            | bookIds ->
                let bookId = bookIds[0] // this assumes no parallel checkouts (no async/await)
                let newCheckout = { BorrowedBy = borrower; BorrowedAt = now; BookId = bookId; ReturnedAt = None }
                { state with Circulations = newCheckout :: state.Circulations }

let returnBookWorkflow : ReturnBookWorkflow =
    fun state bookId borrower now ->
        let circulations = state.Circulations
        let returnableCirculation = circulations |> areCheckedOut |> List.find (fun x -> x.BookId = bookId)
        let returnedCirculation = { returnableCirculation with ReturnedAt = Some now } // TODO check borrower ...
        let otherCirculations = circulations |> List.filter (fun x -> x.BookId <> returnedCirculation.BookId)
        { state with Circulations = returnedCirculation :: otherCirculations }
        
