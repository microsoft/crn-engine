// This file is only for type checking. (FP) These are the types for examples.
interface ExamplesGroup {
    Name: string;
    Correspondence: { [key: string]: string };
}

interface SelectedExample {
    Name: string;
    Group: ExamplesGroup;
}
